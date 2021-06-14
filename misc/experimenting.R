

library(dplyr)
library(R6)
library(tidyr)
library(purrr)
library(mvtnorm)
devtools::load_all()
#
# as_covmat <- function(sig, corr){
#     len <- length(sig)
#     cormat <- diag(rep(1, len))
#     index <- 1
#     for(i in 1:len){
#         for( j in 1:len){
#             if (i < j){
#                 cormat[i,j] <- corr[index]
#                 cormat[j,i] <- corr[index]
#                 index <- index + 1
#             }
#         }
#     }
#     return((sig %*% t(sig)) * cormat)
# }
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





n <- 10
nv <- 3


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



rmd <- rmDataConstructor$new(
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





