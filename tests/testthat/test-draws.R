library(dplyr)
library(testthat)

set.seed(101)

n <- 10
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

dat[sample(1:(nv*n), size = 7), "outcome"] <- NA

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

# function for checking the samples
test_samples_condmean <- function(samples, method) {

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

    # check that sigma is a list of length 2
    sigmas <- lapply(samples, function(x) x$sigma)
    expect_true( all(sapply(sigmas, typeof)  == "list") )
    expect_true( all(sapply(sigmas, length)  == 2) )
}

test_samples_bayes <- function(samples) {
    # test output type
    expect_type(samples, "list")

    # test output type of every sample
    expect_true( all(sapply(samples, typeof) == "list") )

    # check that returned ids are the same as original data
    expect_true( all(sapply(samples,
                            function(x) identical(x$ids, as.character(covars$subjid))) ))

    # check that sigma is a list of length 2
    sigmas <- lapply(samples, function(x) x$sigma)
    expect_true( all(sapply(sigmas, typeof)  == "list") )
    expect_true( all(sapply(sigmas, length)  == 2) )
}

# function for checking output from draws
test_draws <- function(draws_obj, method) {

    # check that output is a list object
    expect_type(draws_obj, "list")

    if(class(method) == "condmean") {
        if(method$type == "bootstrap") {
            # check that length of objects is as expected
            expect_length(draws_obj, 5)
            expect_length(draws_obj$samples, method$n_samples)
            expect_length(draws_obj$optimizers, method$n_samples)

            # check that samples object is as expected
            test_samples_condmean(draws_obj$samples, method)

        } else if(method$type == "jackknife") {
            # check that length of objects is as expected
            expect_length(draws_obj, 5)
            expect_length(draws_obj$samples, n+1)
            expect_length(draws_obj$optimizers, n+1)

            # check that samples object is as expected

            # test separately first element (fit on original sample)
            # from actual jackknife based samples

            test_samples_condmean(draws_obj$samples[-1], method)

            mmrm_initial <- draws_obj$samples[[1]]

            # check that mmrm fit on original data returns equal ids as original data
            expect_equal(mmrm_initial$ids_boot, as.character(covars$subjid))

            # check that sigma is a list of length one (since same_cov = TRUE)
            expect_equal( typeof(mmrm_initial$sigma), "list")
            expect_length( mmrm_initial$sigma, 2)
        }
    } else if(class(method) %in% c("bayes", "approxbayes") ) {
        # check that length of objects is as expected
        ifelse(class(method) == "bayes", expect_length(draws_obj, 4), expect_length(draws_obj, 5))
        expect_length(draws_obj$samples, method$n_samples)

        # check that samples object is as expected
        test_samples_bayes(draws_obj$samples)
    } else {
        stop(paste("class of method is", class(method),": not expected"))
    }
}

test_that(
    "get_bootstrap_samples has expected output",
    {

        method <- list(
            covariance = "ar1",
            threshold = 0.01,
            same_cov = TRUE,
            REML = TRUE,
            n_samples = 3,
            type = "bootstrap"
        )

        # prepare input arguments
        longdata <- longDataConstructor$new(dat, vars)
        model_df <- as_model_df(dat, as_simple_formula(vars))
        scaler <- scalerConstructor$new(model_df)

        # call function
        draws_obj <- get_bootstrap_samples(
            longdata = longdata,
            method = method,
            scaler = scaler
        )

        # check length of object
        expect_length(draws_obj, 2)

        # check length of samples object (n_samples - 1)
        expect_length(draws_obj$samples, 2)

        # check that each element has 5 sub-elements
        expect_true( all(sapply(draws_obj$samples, length) == 5) )

        # check samples
        test_samples_condmean(draws_obj$samples, method)
    }
)

test_that(
    "draws has expected output (approxbayes)",
    {
        method = method_approxbayes(
            covariance = "ar1",
            threshold = 0.01,
            same_cov = TRUE,
            REML = TRUE,
            n_samples = 3
        )

        ############## method: approxbayes
        draws_obj <- draws(
            dat,
            data_ice = NULL,
            vars,
            method = method
        )

        expect_true( all(sapply(draws_obj$samples, length) == 6) )
        test_draws(draws_obj, method)
    }
)

test_that(
    "draws has expected output (bootstrap)",
    {

        method = method_condmean(
            covariance = "ar1",
            threshold = 0.01,
            same_cov = TRUE,
            REML = TRUE,
            n_samples = 3,
            type = "bootstrap"
        )

        ############## method: condmean (bootstrap)
        draws_obj <- draws(
            dat,
            data_ice = NULL,
            vars,
            method = method
        )

        expect_true( all(sapply(draws_obj$samples, length) == 5) )
        test_draws(draws_obj, method)
    }
)

test_that(
    "draws has expected output (jackknife)",
    {

        method = method_condmean(
            covariance = "ar1",
            threshold = 0.01,
            same_cov = TRUE,
            REML = TRUE,
            type = "jackknife"
        )

        ############## method: condmean (jackknife)
        draws_obj <- draws(
            dat,
            data_ice = NULL,
            vars,
            method = method
        )

        expect_true( all(sapply(draws_obj$samples, length) == 5) )
        test_draws(draws_obj, method)
    }
)

test_that(
    "draws has expected output (bayes)",
    {

        method <- method_bayes(
            burn_in = 200,
            burn_between = 2,
            same_cov = TRUE,
            n_samples = 3
        )

        draws_obj <- draws(
            dat,
            data_ice = NULL,
            vars,
            method = method
        )

        expect_true( all(sapply(draws_obj$samples, length) == 3) )
        test_draws(draws_obj, method)
    }
)


test_that("nmar data is removed as expected",{
    # In order to test if nmar is being removed correctly we will
    # create a dataset flag seveal patients as being nmar then compare
    # the output of draws on this dataset vs the same dataset after
    # manually removing those observations

    set.seed(101)

    mysig <- as_covmat(
        sig = c(1, 3, 5),
        corr = c(0.3, 0.5, 0.8)
    )

    dat <- get_sim_data(20, mysig)

    nmar_ids <- sample(unique(dat$id), size = 4)

    dat2 <- dat %>%
        mutate(outcome = if_else(id %in% nmar_ids & visit %in% c("visit_2", "visit_3"), NA_real_, outcome))

    dat_ice <- tibble(
        id = nmar_ids,
        method = "CR",
        visit = "visit_2"
    )

    vars <- list(
        outcome = "outcome",
        visit = "visit",
        method = "method",
        subjid = "id",
        group = "group",
        method = "method",
        covariates = c("age", "sex")
    )

    method <- method_condmean(type = "jackknife")
    d1 <- draws(dat, dat_ice, vars, method)
    d2 <- draws(dat2, dat_ice, vars, method)
    expect_equal(d1$samples, d2$samples)


    method <- method_approxbayes(n_samples = 5)
    set.seed(101)
    d1 <- draws(dat, dat_ice, vars, method)
    set.seed(101)
    d2 <- draws(dat2, dat_ice, vars, method)
    expect_equal(d1$samples, d2$samples)

    method <- method_bayes(
        burn_in = 200,
        burn_between = 2,
        same_cov = TRUE,
        n_samples = 3
    )

    set.seed(101)
    d1 <- draws(dat, dat_ice, vars, method)
    set.seed(101)
    d2 <- draws(dat2, dat_ice, vars, method)
    expect_equal(d1$samples, d2$samples)

})
