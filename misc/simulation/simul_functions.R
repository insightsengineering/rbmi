# ------ Simulate datasets for the conditional mean simulation study ----

simul_data <- function(n=100, H0 = TRUE){

    # n: number of subjects per group
    # H0: If TRUE, simulate under the null hypothesis
    #     If FALSE, simulate under the alternative hypothesis
    #
    # Endpoint to be analyzed in the simulation study is y-y_bl
    #---------------------------------------------------------------------------------------------------
    # sample size per group
    n1 <- n2 <- n
    # visit schedule (every 2 months for 1 year)
    time <- seq(0,12,by=2)
    # mean trajectory control
    mu2 <- 50+10/12*time
    stop_slope_change2 <- rep(0,6)
    # mean trajectory intervention
    if (H0==TRUE){
        mu1 <- mu2
    } else {
        mu1 <- 50+10/12*time-5/12*pmax(time-4,0)
    }
    stop_slope_change1 <- (diff(mu2)-diff(mu1))/2 # division by 2 because diff(.) gives slope over 2 months
    # create Sigma
    sd_intercept <- 5
    sd_slope <- 5
    cor_slopeInter <- 0.25
    sd_error <- 2.5
    covRE <- matrix(c(sd_intercept^2,
                      cor_slopeInter*sd_intercept*sd_slope,
                      cor_slopeInter*sd_intercept*sd_slope,
                      sd_slope^2),ncol=2)
    Sigma <- cbind(1,time/12)%*%covRE%*%rbind(1,time/12)+diag(sd_error^2,nrow=length(time))
    # Simulate data without ICEs
    d1 <- simple_trajectories(mu2 = mu2, Sigma1 = Sigma,mu1 = mu1, Sigma2 = Sigma,n1 = n1, n2 = n2, time = time)
    # Simulate treatment discontinuation ICE
    # probability of treatment discontinuation after each visit
    model <-        ~             1+           I(group=="Intervention")+I(pmax(x-50,0)/10)
    model_coef <-  c(log(0.015/0.985),log(0.025/0.975)-log(0.015/0.985),              1.5)
    d2 <- calc_discontinue_dropout(data = d1,
                                   model =   model,
                                   model_coef = model_coef,
                                   prob_dropout = 0.75)
    # Adjust trajectories post discontinuation
    d3 <- adjust_trajectories(data=d2,
                              stop_slope_change2 = stop_slope_change2,
                              stop_slope_change1 = stop_slope_change1)
    # Reorganize data slightly
    d <- d3 %>%
        mutate(y_bl=x_bl,
               y_noICE=x,
               y_noDropout=y,
               y=ifelse(visit<=dropout_visit,y,NA)) %>%
        dplyr::select(patnum, group, visit, time, trt_stop_visit, dropout_visit, time_off_trt,
                      y_bl, y_noICE, y_noDropout, y)
    # return data
    return(d)
}

# ------ Simulate endpoint trajectories without intercurrent events ----

# mu2, Sigma2: mean and covariance matrix of control group [including baseline]
# mu1, Sigma1: mean and covariance matrix of intervention group
# n1, n2     : Sample size of control (n2) and intervention (n1) group
# time       : Vector of time points [must be of same length mu2]

simple_trajectories <- function(mu2, Sigma2, mu1 = mu2, Sigma1 = Sigma2, n2, n1, time = NULL){
    require(MASS)
    m <- length(mu2) # number of visits per subject (including baseline)
    if (is.null(time)) time <- 0:(m-1)
    # Control group
    r2 <- data.frame("patnum" = rep(1:n2,each=m),
                     "group"    = "Control",
                     "visit"  = rep(0:(m-1),n2),
                     "time"   = rep(time,n2),
                     "x_bl"   =  NA,
                     "x"      = c(t(mvrnorm(n=n2,mu=mu2,Sigma=Sigma2))))
    # Intervention group
    r1 <- data.frame("patnum" = n2+rep(1:n1,each=m),
                     "group"    = "Intervention",
                     "visit"  = rep(0:(m-1),n1),
                     "time"   = rep(time,n1),
                     "x_bl"   = NA,
                     "x"      = c(t(mvrnorm(n=n1,mu=mu1,Sigma=Sigma1))))
    # Pool both groups and add baseline
    r <- rbind(r2,r1)
    r$x_bl <- rep(r$x[time==0],each=m)
    r$group <- factor(r$group,levels=c("Control","Intervention"))
    return(r)
}

# ------ Simulate time of study treatment discontinuation and study dropout ------

# - probability of study treatment discontinuation at/after each visit is simulated according to a logistic model
#   (arguments model and model_coef)
# - prob_dropout is the probability of dropping out from the study immediately after discontinuation

calc_discontinue_dropout <- function(data, model, model_coef, prob_dropout){
    library(tidyverse)
    # Simulate visit of treatment discontinuation
    lp <- c(model.matrix(model,data)%*%model_coef) # linear predictor
    prob_stop_trt <- binomial(link = "logit")$linkinv(lp)
    data$stop_trt <- rbinom(n=length(lp),size=1,prob=prob_stop_trt)
    d_stop <- data %>%
        group_by(patnum) %>%
        summarise(trt_stop_visit = ifelse(any(stop_trt==1),min(visit[stop_trt==1]),Inf),.groups="keep") %>%
        ungroup() %>%
        mutate(dropout_visit = ifelse(sample(c(TRUE,FALSE),size=length(unique(data$patnum)),replace=TRUE,prob=c(prob_dropout,1-prob_dropout)),trt_stop_visit,Inf))
    data$stop_trt <- NULL # drop variable again ("relevant" variable is trt_stop_visit)
    # Merge d_stop back to data and add and derive time since treatment stop
    data <- inner_join(data,d_stop,by="patnum")
    data <- data %>%
        group_by(patnum) %>%
        mutate(time_off_trt = ifelse(trt_stop_visit<=max(visit),
                                     pmax(0,time-time[visit==trt_stop_visit]),0))
    # return dataset
    return(data)
}

# ------ Adjust trajectories according to effects of treatment discontinuation  ----

# - stop_slope_change2 (control) and stop_slope_change1: change in outcome slope following treatment stop
#   [numeric for fixed slope change or a vector of length #visits-1 for time-varying slopes]

adjust_trajectories <- function(data, stop_slope_change2 = 0 , stop_slope_change1 = 0){
    # calculate adjusted trajectory y
    data$control <- (data$group=="Control")
    data$intervention <- (data$group=="Intervention")
    data <- data%>%
        group_by(patnum) %>%
        mutate(y= x +
                   control*cumsum(c(0,diff(time_off_trt))*c(0,stop_slope_change2))+
                   intervention*cumsum(c(0,diff(time_off_trt))*c(0,stop_slope_change1)))
    return(data)
}
