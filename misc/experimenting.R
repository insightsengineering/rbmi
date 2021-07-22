

library(dplyr)
library(R6)
library(tidyr)
library(purrr)
library(mvtnorm)
devtools::load_all()
#
as_covmat <- function(sig, corr){
    len <- length(sig)
    cormat <- diag(rep(1, len))
    index <- 1
    for(i in 1:len){
        for( j in 1:len){
            if (i < j){
                cormat[i,j] <- corr[index]
                cormat[j,i] <- corr[index]
                index <- index + 1
            }
        }
    }
    return((sig %*% t(sig)) * cormat)
}
#
# ## Functions required to create our simulated datasets
# get_sample <- function(mu, sigma){
#     len <- nrow(sigma)
#     z <- rmvnorm(1, rep(mu,len), sigma = sigma)
#     colnames(z) <- paste0("vis", 1:len)
#     as_tibble(z)
# }
#
# get_dat <- function(sigma){
#     n <- 2000
#     dat <- tibble(
#         age = rnorm(n),
#         sex = factor(sample(c("M","F"), size =n, replace = TRUE)),
#         group = factor(sample(c("A", "B"), size = n, replace = TRUE)) ,
#         mu = 20 + 8 * age + (as.numeric(sex) -1) * 3,
#         val = map(mu, get_sample, sigma=sigma)
#     ) %>%
#         unnest(val) %>%
#         mutate(pt = row_number()) %>%
#         gather(visit, outcome, starts_with("vis")) %>%
#         mutate(visit = factor(visit)) %>%
#         mutate(visit_n = as.numeric(visit)) %>%
#         arrange(pt, visit_n)
#
#     return(dat)
# }
#
# sigma <- as_covmat( sig = c(1,2,3) , c(0.5, 0.7, 0.8))
# dat <- get_dat(sigma) %>%
#     mutate(pt = factor(pt))
#

as_covmat( c(1,2,3,4), c(0.2, 0.4, 0.6, 0.8 ,0.4, 0.5)) %>% dput

as_covmat( c(1,2,3), c(0.2, 0.4, 0.6, 0.8 )) %>% dput

n <- 10
nv <- 3

as_covmat <- function(sig, corr){
    len <- length(sig)
    cormat <- diag(rep(1, len))
    index <- 1
    for(i in 1:len){
        for( j in 1:len){
            if (i < j){
                cormat[i,j] <- corr[index]
                cormat[j,i] <- corr[index]
                index <- index + 1
            }
        }
    }
    return((sig %*% t(sig)) * cormat)
}



covars <- tibble(
    id = 1:n,
    age = rnorm(n),
    group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
    sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F")),
    strata = c("A","A","A","A","A","A","A","A","B", "B")
)

dat <- tibble(
    id = rep.int(1:n, nv)
) %>%
    left_join(covars, by = "id") %>%
    mutate( outcome = rnorm(
        n(),
        age * 3 + (as.numeric(sex) - 1) * 3 + (as.numeric(group) - 1) * 4,
        sd = 3
    )) %>%
    arrange(id) %>%
    group_by(id) %>%
    mutate( visit = factor(paste0("Visit ", 1:n())))  %>%
    ungroup() %>%
    mutate(id = as.character(id))


dat[c(1,2,3,4,5,7), "outcome"] <- NA



rmd <- longDataConstructor$new(
    data = dat,
    vars = list(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        strata = "strata"
    )
)

rmd$sample_ids()

implist <- list(
    list( id = "1", values = c(1,2,3)),
    list( id = "1", values = c(4,5,6)),
    list( id = "2", values = c(7,8)),
    list( id = "3", values = c(9)),
    list( id = "1", values = c(10,11,12))
)
rmd$get_data(implist)

time_it <- function(expr){
    start <- Sys.time()
    expr
    stop <- Sys.time()
    difftime(stop, start, units = "secs")
}



implist <- list()
for( i in 1:2000){
    implist[[i]] <- list(
        id = "1",
        values = c(3,4,5)
    )
}

time_it({
    for( i in 1:2000){
        rmd$get_data(implist)
    }
})

library(glmmTMB)



glmmTMB::glmmTMB()

glmmTMB



















library(glmmTMB)
library(dplyr)
library(tidyr)
library(mvtnorm)
devtools::load_all()


set_col_names <- function(x, nam) {
    colnames(x) <- nam
    return(x)
}

f2n <- function(x) as.numeric(x) - 1

as_covmat <- function(sig, corr){
    len <- length(sig)
    cormat <- diag(rep(1, len))
    index <- 1
    for(i in 1:len){
        for( j in 1:len){
            if (i < j){
                cormat[i,j] <- corr[index]
                cormat[j,i] <- corr[index]
                index <- index + 1
            }
        }
    }
    return((sig %*% t(sig)) * cormat)
}


time_it <- function(expr){
    start <- Sys.time()
    expr
    stop <- Sys.time()
    difftime(stop, start, units = "secs")
}

n <- 1000
nv <- 4


covars <- tibble(
    id = 1:n,
    age = rnorm(n),
    group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
    sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F"))
)

mysig <- as_covmat(
    sig = c(1, 3, 5, 7),
    corr = c(0.1,0.2,0.3,0.4,0.5,0.6)
)

dat <- rmvnorm(n, sigma = mysig) %>%
    set_col_names(paste0("visit_", 1:nv)) %>%
    as_tibble() %>%
    mutate(id = 1:n()) %>%
    gather("visit", "outcome", -id) %>%
    mutate(visit = factor(visit)) %>% 
    arrange(id, visit) %>%
    left_join(covars, by = "id") %>% 
    mutate(outcome = outcome + 5 +  3 * age + 3 * f2n(sex) + 4 * f2n(group)) %>% 
    mutate(id = as.character(id))



lm(outcome ~ age + group + sex, data = dat)


rmd <- longDataConstructor$new(
    data = dat,
    vars = list(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group"
    )
)

boot_ids <- rmd$sample_ids()
boot_ids <- boot_ids[order(boot_ids)]

boot_ids_unique <- unique(boot_ids)

dat0 <- rmd$get_data(rmd$ids) 
dat1 <- rmd$get_data(boot_ids)
dat2 <- rmd$get_data(boot_ids_unique)


time_it({
    mod <- glmmTMB(
        outcome ~ sex + group + age + us(0 + visit | id),
        data = dat, 
        dispformula = ~0,
        REML = TRUE, 
        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "L-BFGS-B"))
    )
})


time_it({
    mod0 <- glmmTMB(
        outcome ~ sex + group + age + us(0 + visit | id),
        data = dat0,
        dispformula = ~0,
        REML = TRUE, 
        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "L-BFGS-B"))
    )
})

params <- getME(mod, name = c("beta", "theta"))
params$beta <- params$beta[-length(params$beta)]


time_it({
    mod1 <- glmmTMB(
        outcome ~ sex + group + age + us( 0 + visit|id),
        data = dat1,
        REML = TRUE, 
        start = params, 
        dispformula = ~0,
        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "L-BFGS-B"))
    )
})

weight_vec <- rep(tapply(boot_ids, boot_ids, length)[boot_ids_unique], each = nv)
names(weight_vec) <- NULL

time_it({
    mod2 <- glmmTMB(
        outcome ~ sex + group  + age + us( 0 + visit|id),
        data = dat2,
        REML = TRUE, 
        weights = weight_vec / length(weight_vec) ,
        start = params, 
        dispformula = ~0,
        control = glmmTMBControl(optimizer = optim, optArgs = list(method = "L-BFGS-B"))
    )
})


tmp <- glmmTMB(
    outcome ~ sex + group  + age + us( 0 + visit|id),
    data = dat2,
    REML = TRUE, 
    start = params, 
    dispformula = ~0,
    control = glmmTMBControl(optimizer = optim, optArgs = list(method = "L-BFGS-B")),
    doFit=FALSE
)
tmp$data.tmb$Xd <- cbind(tmp$data.tmb$Xd, matrix(-log(weight_vec / length(weight_vec)), ncol = 1))
tmp$parameters$betad <- c(tmp$parameters$betad, 1)
tmp$mapArg <- list(betad = factor(c(1, NA)))

fit <- glmmTMB:::fitTMB(tmp)


fixef(mod1)$cond
fixef(mod2)$cond


VarCorr(mod1)
VarCorr(mod2)



dat <- haven::read_sas("~/Downloads/chapter15_example.sas7bdat")

dat2 <- dat %>%
    mutate(VISIT = factor(VISIT)) %>%
    mutate(PATIENT = factor(PATIENT)) 

dat_exp <- expand(
    dat2,
    VISIT = unique(dat2$VISIT),
    PATIENT = unique(dat2$PATIENT),
    .fill_vars = c("basval", "THERAPY"),
    .fill_group = c("PATIENT")
)

