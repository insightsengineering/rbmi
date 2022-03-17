suppressPackageStartupMessages({
    library(dplyr)
})



test_that("Results are Reproducible", {

    skip_if_not(is_nightly())

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
        imputeobj <- impute( draws = drawobj, references = c("A" = "B", "B" = "B"))
        anaobj <- analyse( imputeobj, fun = rbmi::ancova, vars = vars2)
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



test_that("bayes - seed argument works without set.seed", {
    
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
        seed = 1482,
        burn_between = 5,
        burn_in = 200,
        n_samples = 2
    )

    set.seed(49812)
    x <- suppressWarnings({
        draws(dat, dat_ice, vars, meth, quiet = TRUE)
    })
    set.seed(2414)
    y <- suppressWarnings({
        draws(dat, dat_ice, vars, meth, quiet = TRUE)
    })
    expect_equal(x$samples, y$samples)
})
