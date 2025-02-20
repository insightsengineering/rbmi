suppressPackageStartupMessages({
    library(dplyr)
})



test_that("Results are Reproducible", {

    skip_if_not(is_full_test())

    run_test <- function(method) {
        set.seed(4642)
        sigma <- as_vcov(c(2, 1, 0.7), c(0.5, 0.3, 0.2))
        dat <- get_sim_data(40, sigma, trt = 8) %>%
            mutate(outcome = if_else(rbinom(n(), 1, 0.3) == 1, NA_real_, outcome))

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

        vars2 <- vars
        vars2$covariates <- c("age", "sex")

        # Force the resolution of any random draws within the method object
        # so that they don't advance the seed when called within `draws()`
        # see #482 for further details
        force(method)

        set.seed(984)
        drawobj <- suppressWarnings({
            draws(
                data = dat,
                data_ice = dat_ice,
                vars = vars,
                method = method,
                quiet = TRUE
            )
        })
        imputeobj <- impute(draws = drawobj, references = c("A" = "B", "B" = "B"))
        anaobj <- analyse(imputeobj, fun = rbmi::ancova, vars = vars2)
        poolobj <- pool(results = anaobj)


        set.seed(984)
        drawobj2 <- suppressWarnings({
            draws(
                data = dat,
                data_ice = dat_ice,
                vars = vars,
                method = method,
                quiet = TRUE
            )
        })
        imputeobj2 <- impute(draws = drawobj2, references = c("A" = "B", "B" = "B"))
        anaobj2 <- analyse(imputeobj2, fun = rbmi::ancova, vars = vars2)
        poolobj2 <- pool(results = anaobj2)

        ## Tidy up things that will never be the same:
        drawobj$formula <- NULL # Formulas contain environments specific to their build
        drawobj2$formula <- NULL
        drawobj$fit <- NULL  # Bayes object has "fit" which contains a timestamp
        drawobj2$fit <- NULL
        anaobj$call <- NULL   # Argument names are different (imputeobj2)
        anaobj2$call <- NULL

        expect_equal(drawobj, drawobj2)
        expect_equal(imputeobj, imputeobj2)
        expect_equal(anaobj, anaobj2)
        expect_equal(poolobj, poolobj2)
    }

    run_test(method_approxbayes(n_samples = 5))
    run_test(method_condmean(n_samples = 5))
    run_test(method_bayes(n_samples = 5))
    run_test(method_condmean(type = "jackknife"))
})



test_that("bayes - set.seed produces identical results", {

    sigma <- as_vcov(c(2, 1, 0.7), c(0.5, 0.3, 0.2))
    dat <- get_sim_data(200, sigma, trt = 8) %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.3) == 1, NA_real_, outcome))

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

    meth <- method_bayes(
        n_samples = 6,
        control = control_bayes(
            warmup = 200,
            thin = 5
        )
    )

    set.seed(1234)
    x <- suppressWarnings({
        draws(dat, dat_ice, vars, meth, quiet = TRUE)
    })
    set.seed(1234)
    y <- suppressWarnings({
        draws(dat, dat_ice, vars, meth, quiet = TRUE)
    })
    expect_equal(x$samples, y$samples)
})


test_that("Results are if model is recompiled", {

    skip_if_not(is_full_test())

    run_test <- function() {
        set.seed(4642)
        sigma <- as_vcov(c(2, 1, 0.7), c(0.5, 0.3, 0.2))
        dat <- get_sim_data(40, sigma, trt = 8) %>%
            mutate(outcome = if_else(rbinom(n(), 1, 0.3) == 1, NA_real_, outcome))

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

        vars2 <- vars
        vars2$covariates <- c("age", "sex")

        set.seed(984)
        drawobj <- suppressWarnings({
            draws(
                data = dat,
                data_ice = dat_ice,
                vars = vars,
                method = method_bayes(n_samples = 20),
                quiet = TRUE
            )
        })
        imputeobj <- impute(draws = drawobj, references = c("A" = "B", "B" = "B"))
        anaobj <- analyse(imputeobj, fun = rbmi::ancova, vars = vars2)
        poolobj <- pool(results = anaobj)

        ## Tidy up things that will never be the same:
        drawobj$formula <- NULL # Formulas contain environments specific to their build
        drawobj$fit <- NULL     # Bayes object has "fit" which contains a timestamp
        anaobj$call <- NULL     # Argument names are different (imputeobj2)

        return(list(
            draws = drawobj,
            impute = imputeobj,
            analyse = anaobj,
            pool = poolobj
        ))
    }

    old_cache <- options("rbmi.cache_dir")
    tmp_dir <- tempfile(tmpdir = tempdir(check = TRUE))
    dir.create(tmp_dir)
    options("rbmi.cache_dir" = tmp_dir)
    results_no_cache <- run_test()
    results_cache <- run_test()  # Now rerun but using the same cache
    options("rbmi.cache_dir" = old_cache)

    expect_equal(results_no_cache$draws, results_cache$draws)
    expect_equal(results_no_cache$impute, results_cache$impute)
    expect_equal(results_no_cache$analyse, results_cache$analyse)
    expect_equal(results_no_cache$pool, results_cache$pool)

})
