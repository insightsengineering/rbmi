library(dplyr)
library(testthat)

set.seed(101)

n <- 16
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
    covariance = "ar1",
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_samples = 3,
    type = "bootstrap"
)

# function for checking output from get_bootstrap_samples (and jackknife)
test_boot_samples <- function(samples, method) {

    # test output type
    expect_type(samples, "list")

    # test output type of every sample
    expect_true( all(sapply(samples, typeof) == "list") )

    # test length of ids equals sample size (in case of jackknife we can check for actual ids)
    if(method$type == "bootstrap") {
        expect_true( all(sapply(samples, function(x) length(x$ids_boot)) == n) )
    } else if(method$type == "jackknife") {
        expect_true( all(sapply(1:length(samples),
                                function(i) identical(samples[[i]]$ids_boot, as.character(covars$subjid[-i]))) ))
    }

    # check that converged is logical
    converged <- sapply(samples, function(x) x$converged)

    expect_true( all(converged %in% c(TRUE, FALSE)) )

    # check that sigma is a list of length one (since same_cov = TRUE)
    sigmas <- lapply(samples, function(x) x$sigma)
    expect_true( all(sapply(sigmas, typeof)  == "list") )
    expect_true( all(sapply(sigmas, length)  == 1) )
}

# function for checking output from draws_bootstrap
test_draws_condmean_and_approxbayes <- function(draws_boot, method) {

    # check that output is a list object
    expect_type(draws_boot, "list")

    # check that length of objects is as expected
    expect_length(draws_boot, 5)

    if(method$type == "bootstrap") {
        expect_length(draws_boot$samples, method$n_samples)
        expect_length(draws_boot$optimizers, method$n_samples)
    } else if(method$type == "jackknife") {
        expect_length(draws_boot$samples, n+1)
        expect_length(draws_boot$optimizers, n+1)
    }

    # check that samples object is as expected
    if(method$type == "bootstrap") {

        test_boot_samples(draws_boot$samples, method)

    } else if(method$type == "jackknife") {
        # test separately first element (fit on original sample)
        # from actual jackknife based samples

        test_boot_samples(draws_boot$samples[-1], method)

        mmrm_initial <- draws_boot$samples[[1]]

        # check that mmrm fit on original data return equal ids as original data
        expect_equal(mmrm_initial$ids_boot, as.character(covars$subjid))

        # check that sigma is a list of length one (since same_cov = TRUE)
        expect_equal( typeof(mmrm_initial$sigma), "list")
        expect_length( mmrm_initial$sigma, 1)
    }
}

test_that(
    "get_bootstrap_samples has expected output",
    {

        # prepare input arguments
        longdata <- longDataConstructor$new(dat, vars)
        model_df <- as_model_df(dat, as_simple_formula(vars))
        scaler <- scalerConstructor$new(model_df)

        # call function
        draws_boot <- get_bootstrap_samples(
            longdata = longdata,
            method = method,
            scaler = scaler
        )

        # check length of object
        expect_length(draws_boot, 2)

        # check length of samples object (n_samples - 1)
        expect_length(draws_boot$samples, 2)

        # check that each element has 5 sub-elements
        expect_true( all(sapply(draws_boot$samples, length) == 5) )

        # check samples
        test_boot_samples(draws_boot$samples, method)
    }
)

test_that(
    "get_jackknife_samples has expected output",
    {

        # prepare input arguments
        longdata <- longDataConstructor$new(dat, vars)
        model_df <- as_model_df(dat, as_simple_formula(vars))
        scaler <- scalerConstructor$new(model_df)

        method$type = "jackknife"

        # call function
        draws_boot <- get_jackknife_samples(
            longdata = longdata,
            method = method,
            scaler = scaler
        )

        # check length of object
        expect_length(draws_boot, 2)

        # check length (number of subjects)
        expect_length(draws_boot$samples, n)

        # check that each element has 5 sub-elements
        expect_true( all(sapply(draws_boot$samples, length) == 5) )

        # check samples
        test_boot_samples(draws_boot$samples, method)
    }
)

test_that(
    "wrapper function draws_bootstrap has expected output (bootstrap)",
    {

        # call function
        draws_boot <- draws_bootstrap(
            data = dat,
            data_ice = NULL,
            vars = vars,
            method = method
        )

        # check output
        expect_true( all(sapply(draws_boot$samples, length) == 5) )
        test_draws_condmean_and_approxbayes(draws_boot, method)

    })

test_that(
    "wrapper function draws_bootstrap has expected output (jackknife)",
    {

        # prepare input arguments
        method$type = "jackknife"

        # call function
        draws_boot <- draws_bootstrap(
            data = dat,
            data_ice = NULL,
            vars = vars,
            method = method
        )

        # check output
        expect_true( all(sapply(draws_boot$samples, length) == 5) )
        test_draws_condmean_and_approxbayes(draws_boot, method)

    })

test_that(
    "draws has expected output for condmean and approxbayes",
    {
        ############## method: approxbayes
        draws_boot <- draws(
            dat,
            data_ice = NULL,
            vars,
            method = method_approxbayes(
                covariance = "ar1",
                threshold = 0.01,
                same_cov = TRUE,
                REML = TRUE,
                n_imputations = 3
            )
        )

        ids <- lapply(
            draws_boot$samples,
            function(x) x$ids
        )

        expect_true( all(sapply(ids, function(x) identical(x, unique(dat$subjid)))) )
        expect_true( all(sapply(draws_boot$samples, length) == 6) )
        test_draws_condmean_and_approxbayes(draws_boot, method)

        ############## method: condmean
        draws_boot <- draws(
            dat,
            data_ice = NULL,
            vars,
            method = method_condmean(
                covariance = "ar1",
                threshold = 0.01,
                same_cov = TRUE,
                REML = TRUE,
                n_samples = 3,
                type = "bootstrap"
            )
        )

        expect_true( all(sapply(draws_boot$samples, length) == 5) )
        test_draws_condmean_and_approxbayes(draws_boot, method)
    }
)



