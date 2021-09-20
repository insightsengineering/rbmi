suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})

get_data <- function(n) {
    sigma <- as_vcov(c(2, 1, 0.7), c(0.5, 0.3, 0.2))

    set.seed(1518)

    dat <- get_sim_data(n, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss) %>%
        mutate(group = factor(group, labels = c("Placebo", "TRT")))


    dat_ice <- dat %>%
        group_by(id) %>%
        arrange(id, visit) %>%
        filter(is.na(outcome)) %>%
        slice(1) %>%
        ungroup() %>%
        select(id, visit) %>%
        mutate(strategy = "JR")


    vars <- set_vars(
        outcome = "outcome",
        group = "group",
        strategy = "strategy",
        subjid = "id",
        visit = "visit",
        covariates = c("age", "sex", "visit * group")
    )
    list(dat = dat, dat_ice = dat_ice, vars = vars)
}


standard_checks <- function(dobj, d, meth) {
    expect_true(all(c("samples", "formula", "data", "method", "fit", "n_failures") %in% names(dobj)))
    expect_true(class(dobj)[[1]] == "draws")
    expect_true(class(dobj$formula) == "formula")
    expect_equal(meth, dobj$method)
    expect_true(all(vapply(dobj$samples, function(x) !x$failed, logical(1))))
    expect_true(class(dobj$samples)[[1]] == "sample_list")
    expect_true(all(vapply(dobj$samples, function(x) length(x$sigma) == 2, logical(1))))
    expect_true(all(vapply(dobj$samples, function(x) identical(x$sigma[[1]], x$sigma[[2]]), logical(1))))
    expect_true(all(vapply(dobj$samples, function(x) length(x$beta) == 8, logical(1))))
    expect_true(all(vapply(dobj$samples, function(x) all(!is.na(x$beta)), logical(1))))

    validate(dobj$samples)
    for (samp in dobj$samples) {
        validate(samp)
    }
}


test_that("approxbayes", {

    set.seed(40123)
    d <- get_data(40)
    meth <- method_approxbayes(n_samples = 6)
    dobj <- draws(d$dat, d$dat_ice, d$vars, meth)
    standard_checks(dobj, d, meth)

    expect_length(dobj$samples, 6)
    expect_true(all(vapply(dobj$samples, function(x) all(x$ids == levels(d$dat$id)), logical(1))))
})


test_that("condmean - bootstrap", {

    set.seed(40123)
    d <- get_data(40)
    meth <- method_condmean(n_samples = 5)
    dobj <- draws(d$dat, d$dat_ice, d$vars, meth)
    standard_checks(dobj, d, meth)

    expect_length(dobj$samples, 6)
    expect_equal(dobj$samples[[1]]$ids, levels(d$dat$id))
    expect_true(all(vapply(dobj$samples[-1], function(x) any(x$ids != levels(d$dat$id)), logical(1))))

    set.seed(623)
    meth <- method_condmean(n_samples = 5)
    dobj2 <- draws(d$dat, d$dat_ice, d$vars, meth)
    standard_checks(dobj2, d, meth)

    expect_length(dobj2$samples, 6)
    expect_equal(dobj2$samples[[1]], dobj$samples[[1]])
    expect_true(!identical(dobj$samples[[2]], dobj2$samples[[2]]))
})



test_that("condmean - jackknife", {

    set.seed(40123)
    d <- get_data(20)
    meth <- method_condmean(type = "jackknife")
    dobj <- draws(d$dat, d$dat_ice, d$vars, meth)
    standard_checks(dobj, d, meth)

    expect_length(dobj$samples, 21)
    expect_equal(dobj$samples[[1]]$ids, levels(d$dat$id))
    expect_true(all(vapply(dobj$samples[-1], function(x) length(x$ids) == 19, logical(1))))
    for (i in 1:20) {
        expect_equal(dobj$samples[-1][[i]]$ids, levels(d$dat$id)[-i])
    }

    set.seed(123)
    dobj2 <- draws(d$dat, d$dat_ice, d$vars, meth)
    expect_equal(dobj[c("samples", "data")], dobj2[c("samples", "data")])
})


test_that("bayes", {
    set.seed(40123)
    d <- get_data(40)
    meth <- method_bayes(n_samples = 7, burn_in = 200, burn_between = 2, verbose = FALSE)
    dobj <- suppressWarnings({
        draws(d$dat, d$dat_ice, d$vars, meth)
    })
    standard_checks(dobj, d, meth)

    expect_length(dobj$samples, 7)
    expect_true(all(vapply(dobj$samples, function(x) all(x$ids == levels(d$dat$id)), logical(1))))

})


test_that("failure limits", {

    time_it_catch_error <- function(expr) {
        start <- Sys.time()
        tryCatch(expr, error = function(x) list())
        stop <- Sys.time()
        as.numeric(difftime(stop, start, units = "secs"))
    }

    set.seed(40123)
    d <- get_data(15)

    meth_01 <- method_approxbayes(n_samples = 10, threshold = 0.1)
    meth_05 <- method_approxbayes(n_samples = 10, threshold = 0.5)
    meth_10 <- method_approxbayes(n_samples = 10, threshold = 1.0)

    expect_error(draws(d$dat, d$dat_ice, d$vars, meth_01), "More than 1 failed fits")
    expect_error(draws(d$dat, d$dat_ice, d$vars, meth_05), "More than 5 failed fits")
    expect_error(draws(d$dat, d$dat_ice, d$vars, meth_10), "More than 10 failed fits")

    t_01 <- time_it_catch_error(draws(d$dat, d$dat_ice, d$vars, meth_01))
    t_05 <- time_it_catch_error(draws(d$dat, d$dat_ice, d$vars, meth_05))
    t_10 <- time_it_catch_error(draws(d$dat, d$dat_ice, d$vars, meth_10))

    expect_true(t_01 < t_05)
    expect_true(t_05 < t_10)
})


#### TODO - Draw functions to test
# get_bootstrap_draws <- function(longdata, method, use_samp_ids = FALSE, first_sample_orig = FALSE)
# get_jackknife_draws <- function(longdata, method)
# get_mmrm_sample <- function(ids, longdata, method)
# extract_data_nmar_as_na <- function(longdata)
# as_sample_single <- function(...)
# validate.sample_single <- function(...)
# as_sample_list  <- function(...)
# validate.sample_list <- function(x, ...)
# as_draws <- function(method, samples, data, formula, n_failures = NA, fit = NA)






test_that("nmar data is removed as expected", {
    # In order to test if nmar is being removed correctly we will
    # create a dataset flag seveal patients as being nmar then compare
    # the output of draws on this dataset vs the same dataset after
    # manually removing those observations

    set.seed(101)

    mysig <- as_vcov(
        sd = c(1, 3, 5),
        cor = c(0.3, 0.5, 0.8)
    )

    dat <- get_sim_data(20, mysig)

    nmar_ids <- sample(unique(dat$id), size = 4)

    dat2 <- dat %>%
        mutate(outcome = if_else(id %in% nmar_ids & visit %in% c("visit_2", "visit_3"), NA_real_, outcome))

    dat_ice <- tibble(
        id = nmar_ids,
        strategy = "CR",
        visit = "visit_2"
    )

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        strategy = "strategy",
        covariates = c("age", "sex")
    )

    method <- method_condmean(type = "jackknife")
    d1 <- draws(dat, dat_ice, vars, method)
    d2 <- draws(dat2, dat_ice, vars, method)
    expect_equal(d1$samples, d2$samples)

})


test_that("NULL data_ice works uses MAR by default", {

    set.seed(314)
    dat <- simulate_data(n = 100)

    dat2 <- dat %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.2) == 0, outcome, NA_real_))

    dobj <- draws(
        dat2,
        method = method_condmean(n_samples = 5),
        vars = set_vars(
            outcome = "outcome",
            visit = "visit",
            subjid = "id",
            group = "group",
            strategy = "strategy",
            covariates = c("age", "sex")
        )
    )

    expect_true(all(unlist(dobj$data$is_mar)))
    expect_true(all(!unlist(dobj$data$is_post_ice)))
})





