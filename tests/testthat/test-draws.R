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
    dobj <- draws(d$dat, d$dat_ice, d$vars, meth, quiet = TRUE)
    standard_checks(dobj, d, meth)

    expect_length(dobj$samples, 6)
    for (samp in dobj$samples) {
        expect_equal(samp$ids, levels(d$dat$id))
    }
})


test_that("condmean - bootstrap", {

    set.seed(40123)
    d <- get_data(40)
    meth <- method_condmean(n_samples = 5)
    dobj <- draws(d$dat, d$dat_ice, d$vars, meth, quiet = TRUE)
    standard_checks(dobj, d, meth)

    expect_length(dobj$samples, 6)
    expect_equal(dobj$samples[[1]]$ids, levels(d$dat$id))

    for (samp in dobj$samples[-1]) {
        expect_true(max(tapply(samp$ids, samp$ids, length)) >= 2)
        expect_true(length(samp$ids) == length(dobj$samples[[1]]$ids))
    }

    set.seed(623)
    meth <- method_condmean(n_samples = 5)
    dobj2 <- draws(d$dat, d$dat_ice, d$vars, meth, quiet = TRUE)
    standard_checks(dobj2, d, meth)

    expect_length(dobj2$samples, 6)
    expect_equal(dobj2$samples[[1]], dobj$samples[[1]])
    expect_true(!identical(dobj$samples[[2]], dobj2$samples[[2]]))
})



test_that("condmean - jackknife", {

    set.seed(40123)
    d <- get_data(20)
    meth <- method_condmean(type = "jackknife")
    dobj <- draws(d$dat, d$dat_ice, d$vars, meth, quiet = TRUE)
    standard_checks(dobj, d, meth)

    expect_length(dobj$samples, 21)
    expect_equal(dobj$samples[[1]]$ids, levels(d$dat$id))
    expect_true(all(vapply(dobj$samples[-1], function(x) length(x$ids) == 19, logical(1))))
    for (i in 1:20) {
        expect_equal(dobj$samples[-1][[i]]$ids, levels(d$dat$id)[-i])
    }

    set.seed(123)
    dobj2 <- draws(d$dat, d$dat_ice, d$vars, meth, quiet = TRUE)
    expect_equal(dobj[c("samples", "data")], dobj2[c("samples", "data")])
})




test_that("bayes", {
    set.seed(40123)
    d <- get_data(40)
    meth <- method_bayes(n_samples = 7, burn_in = 200, burn_between = 2)
    dobj <- suppressWarnings({
        draws(d$dat, d$dat_ice, d$vars, meth, quiet = TRUE)
    })
    standard_checks(dobj, d, meth)

    expect_length(dobj$samples, 7)
    expect_true(all(vapply(dobj$samples, function(x) all(x$ids == levels(d$dat$id)), logical(1))))

})




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
    d1 <- draws(dat, dat_ice, vars, method, quiet = TRUE)
    d2 <- draws(dat2, dat_ice, vars, method, quiet = TRUE)
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
        quiet = TRUE,
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




test_that("Failure is handled properly", {

    bign <- 75
    sigma <- as_vcov(
        c(2, 1, 0.7),
        c(
            0.3,
            0.4, 0.2
        )
    )

    dat <- get_sim_data(bign, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss)

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

    MockLongData <- R6Class("MockLongData",
        inherit = longDataConstructor,
        public = list(
            failure_index = 0,
            tracker = 0,
            sample_ids = function() {
                self$tracker = self$tracker + 1
                if (self$tracker %in% self$failure_index) {
                    return(c("1", "2"))
                }
                super$sample_ids()
            },
            set_failed_sample_index = function(x) {
                self$tracker = 0
                self$failure_index = x
            }
        )
    )

    ld <- MockLongData$new(dat, vars)
    ld$set_strategies(dat_ice)


    ##################
    #
    # Bootstrap - 1 core
    #

    method <- method_approxbayes(n_samples = 10, threshold = 0.5)
    ld$set_failed_sample_index(seq_len(4))
    stack <- get_bootstrap_stack(ld, method)
    x <- get_draws_mle(
        longdata = ld,
        method = method,
        n_target_samples = method$n_samples,
        failure_limit = (method$threshold * method$n_samples),
        use_samp_ids = FALSE,
        ncores = 1,
        first_sample_orig = FALSE,
        quiet = TRUE,
        sample_stack = stack
    )
    expect_equal(x$n_failures, 4)
    expect_equal(ld$tracker, 15)
    expect_length(stack$stack, 1)



    method <- method_approxbayes(n_samples = 10, threshold = 0.3)
    ld$set_failed_sample_index(2:5)
    stack <- get_bootstrap_stack(ld, method)
    expect_error(
        get_draws_mle(
            longdata = ld,
            method = method,
            n_target_samples = method$n_samples,
            failure_limit = (method$threshold * method$n_samples),
            use_samp_ids = FALSE,
            ncores = 1,
            quiet = TRUE,
            first_sample_orig = FALSE,
            sample_stack = stack
        )
    )
    expect_equal(ld$tracker, 13)
    expect_length(stack$stack, 8)



    ##################
    #
    # Bootstrap - 2 core
    #

    method <- method_approxbayes(n_samples = 10, threshold = 0.5)
    ld$set_failed_sample_index(seq_len(4))
    stack <- get_bootstrap_stack(ld, method)
    x <- get_draws_mle(
        longdata = ld,
        method = method,
        n_target_samples = method$n_samples,
        failure_limit = (method$threshold * method$n_samples),
        use_samp_ids = FALSE,
        ncores = 2,
        quiet = TRUE,
        first_sample_orig = FALSE,
        sample_stack = stack
    )
    expect_equal(x$n_failures, 4)
    expect_equal(ld$tracker, 15)
    expect_length(stack$stack, 1)




    method <- method_approxbayes(n_samples = 10, threshold = 0.3)
    ld$set_failed_sample_index(2:5)
    stack <- get_bootstrap_stack(ld, method)
    expect_error(
        get_draws_mle(
            longdata = ld,
            method = method,
            n_target_samples = method$n_samples,
            failure_limit = (method$threshold * method$n_samples),
            use_samp_ids = FALSE,
            ncores = 2,
            quiet = TRUE,
            first_sample_orig = FALSE,
            sample_stack = stack
        )
    )
    expect_equal(ld$tracker, 13)
    expect_length(stack$stack, 13 - 6)


    ##################
    #
    # Jackknife - failures only
    #

    method <- method_condmean(type = "jackknife")
    stack <- get_jackknife_stack(ld)
    for (i in 5:10) {
        stack$stack[[i]] <- c("1", "2")
    }
    expect_error(
        x <- get_draws_mle(
            longdata = ld,
            method = method,
            n_target_samples = length(ld$ids),
            failure_limit = 0,
            use_samp_ids = FALSE,
            ncores = 1,
            first_sample_orig = FALSE,
            sample_stack = stack,
            quiet = TRUE
        ),
        regexp = "after removing subject '3'"
    )
    expect_length(stack$stack, length(ld$ids) - 5)


    method <- method_condmean(type = "jackknife")
    stack <- get_jackknife_stack(ld)
    for (i in 5:10) {
        stack$stack[[i]] <- c("1", "2", "3")
    }
    expect_error(
        x <- get_draws_mle(
            longdata = ld,
            method = method,
            n_target_samples = length(ld$ids),
            failure_limit = 0,
            use_samp_ids = FALSE,
            ncores = 2,
            first_sample_orig = FALSE,
            sample_stack = stack,
            quiet = TRUE
        ),
        regex = "after removing subject '4'"
    )
    expect_length(stack$stack, length(ld$ids) - 6)

})






test_that("draws is calling get_mmrm_sample properly", {
    bign <- 75
    sigma <- as_vcov(
        c(2, 1, 0.7),
        c(
            0.3,
            0.4, 0.2
        )
    )

    dat <- get_sim_data(bign, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss)

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


    ##################################
    #
    #  Conditional Mean
    #


    method <- method_condmean(n_samples = 2)
    ld <- longDataConstructor$new(dat, vars)
    ld$set_strategies(dat_ice)
    x <- draws(dat, dat_ice, vars, method, quiet = TRUE)

    s1 <- get_mmrm_sample(
        ids = ld$ids,
        longdata = ld,
        method = method,
        optimizer = c("L-BFGS-B", "BFGS")
    )
    expect_equal(ld$ids, x$samples[[1]]$ids)
    expect_equal(s1, x$samples[[1]])


    s2 <- get_mmrm_sample(
        ids = x$samples[[2]]$ids,
        longdata = ld,
        method = method,
        optimizer = optimizer <- list(
            "L-BFGS-B" = NULL,
            "BFGS" = s1[c("beta", "theta")]
        )
    )
    expect_true(!all(ld$ids %in% x$samples[[2]]$ids))
    expect_true(length(ld$ids) == length(x$samples[[2]]$ids))
    expect_equal(s2, x$samples[[2]])




    ##################################
    #
    #  Approx Bayesian
    #


    method <- method_approxbayes(n_samples = 2)
    ld <- longDataConstructor$new(dat, vars)
    ld$set_strategies(dat_ice)
    x <- draws(dat, dat_ice, vars, method, quiet = TRUE)

    s0 <- get_mmrm_sample(
        ids = ld$ids,
        longdata = ld,
        method = method,
        optimizer = c("L-BFGS-B", "BFGS")
    )

    s1 <- get_mmrm_sample(
        ids = x$samples[[1]]$ids_samp,
        longdata = ld,
        method = method,
        optimizer = optimizer <- list(
            "L-BFGS-B" = NULL,
            "BFGS" = s0[c("beta", "theta")]
        )
    )
    s1$ids <- ld$ids
    expect_true(!all(ld$ids %in% x$samples[[1]]$ids_samp))
    expect_true(all(ld$ids %in% x$samples[[1]]$ids))
    expect_true(length(ld$ids) == length(x$samples[[1]]$ids))
    expect_equal(s1, x$samples[[1]])



    s2 <- get_mmrm_sample(
        ids = x$samples[[2]]$ids_samp,
        longdata = ld,
        method = method,
        optimizer = optimizer <- list(
            "L-BFGS-B" = NULL,
            "BFGS" = s0[c("beta", "theta")]
        )
    )
    s2$ids <- ld$ids
    expect_true(!all(ld$ids %in% x$samples[[2]]$ids_samp))
    expect_true(all(ld$ids %in% x$samples[[2]]$ids))
    expect_true(length(ld$ids) == length(x$samples[[2]]$ids))
    expect_equal(s2, x$samples[[2]])
})



test_that("draws.bmlmi works as expected", {

    bign <- 60
    sigma <- as_vcov(
        c(2, 1, 0.7),
        c(
            0.3,
            0.4, 0.2
        )
    )

    dat <- get_sim_data(bign, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss)

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

    set.seed(3013)
    x1 <- draws(
        quiet = TRUE,
        dat,
        dat_ice,
        vars,
        method = method_bmlmi(B = 21),
        ncores = 2
    )

    set.seed(3013)
    x2 <- draws(
        quiet = TRUE,
        dat,
        dat_ice,
        vars,
        method = method_approxbayes(n_samples = 21)
    )

    ### BMLMI should be identical to approx bayes within draws except for sample ids
    expect_equal(
        lapply(x1$samples, function(x) x[c("beta", "sigma", "theta", "failed")]),
        lapply(x2$samples, function(x) x[c("beta", "sigma", "theta", "failed")])
    )

    ### Bootstrapped sample ids should have the same subject appearing multiple times
    ### in each sample
    for (samp in x1$samples) {
        expect_true(max(tapply(samp$ids, samp$ids, length)) > 1)
    }
})




test_that("quiet supress progress messages", {
    
    bign <- 60
    sigma <- as_vcov(
        c(2, 1, 0.7),
        c(
            0.3,
            0.4, 0.2
        )
    )

    dat <- get_sim_data(bign, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss)

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

    set.seed(3013)
    x <- capture.output({
        dobj <- draws(
            data = dat,
            data_ice = dat_ice,
            vars = vars,
            method = method_approxbayes(n_samples = 3)
        )
    })
    expect_true(any(grepl("Estimated running time", x)))
    expect_true(any(grepl("Progress: ", x)))


    set.seed(3013)
    x <- capture.output({
        dobj <- draws(
            data = dat,
            quiet = TRUE,
            data_ice = dat_ice,
            vars = vars,
            method = method_approxbayes(n_samples = 3)
        )
    })
    expect_true(length(x) == 0 & is.character(x))
})
