library(dplyr)
library(testthat)

n <- 20
nv <- 3

covars <- tibble(
    subjid = 1:n,
    age = rnorm(n),
    group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
    sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F")),
    strata = c(rep("A",n/2), rep("B", n/2))
)

dat <- tibble(
    subjid = rep.int(1:n, nv)
) %>%
    left_join(covars, by = "subjid") %>%
    mutate( outcome = rnorm(
        n(),
        age * 3 + (as.numeric(sex) - 1) * 3 + (as.numeric(group) - 1) * 4,
        sd = 3
    )) %>%
    arrange(subjid) %>%
    group_by(subjid) %>%
    mutate( visit = factor(paste0("Visit ", 1:n())))  %>%
    ungroup() %>%
    mutate(subjid = as.character(subjid))

#dat[c(1,2,3,4,6,7), "outcome"] <- NA


vars <- list(
    outcome = "outcome",
    visit = "visit",
    subjid = "subjid",
    group = "group",
    strata = "strata",
    covariates = c("sex", "age"),
    method = "method"
)

data_ice <- NULL

method <- list(
    covariance = c("us"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_imputations = 3
)


test_that(
    "draws using bootstrap has expected output",
    {

draws_params <- draws_bootstrap(
    data = dat,
    data_ice = NULL,
    vars = vars,
    method = method
)
 expect_length(draws_params, 3)
})



