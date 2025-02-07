suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(testthat)
})


### Pre-recorded print objects

test_that("print - Pool Method", {
    expect_snapshot(print(.test_print$bayes$pool), cran = TRUE)
    expect_snapshot(print(.test_print$approxbayes$pool), cran = TRUE)
    expect_snapshot(print(.test_print$condmean_boot$pool$percentile), cran = TRUE)
    expect_snapshot(print(.test_print$condmean_boot$pool$normal), cran = TRUE)
    expect_snapshot(print(.test_print$condmean_jack$pool), cran = TRUE)
    expect_snapshot(print(.test_print$bmlmi$pool), cran = TRUE)
})




test_print_get_data <- function(n) {
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




test_that("print - approx bayes", {
    set.seed(491)
    dobj <- test_print_get_data(40)

    drawobj_ab <- draws(
        data = dobj$dat,
        data_ice = dobj$dat_ice,
        vars = dobj$vars,
        method = method_approxbayes(
            n_samples = 3,
            threshold = 0.5,
            same_cov = TRUE,
            REML = TRUE,
            covariance = "ar1"
        ),
        quiet = TRUE
    )
    expect_snapshot(print(drawobj_ab), cran = TRUE)

    impute_ab <- impute(
        drawobj_ab,
        references = c("TRT" = "Placebo", "Placebo" = "Placebo"),
    )
    expect_snapshot(print(impute_ab), cran = TRUE)

    v2 <- dobj$vars
    v2$covariates <- c("sex*age")
    analysis_ab <- analyse(
        impute_ab,
        vars = v2
    )
    expect_equal(analysis_ab$fun_name, "ancova")
    expect_snapshot(print(analysis_ab), cran = TRUE)
})





test_that("print - bayesian", {
    set.seed(413)
    dobj <- test_print_get_data(40)

    suppressWarnings({
        drawobj_b <- draws(
            data = dobj$dat,
            data_ice = dobj$dat_ice,
            vars = dobj$vars,
            method = method_bayes(
                n_samples = 50,
                control = control_bayes(
                    thin = 1,
                    init = function (chain_id) {
                        list(
                            b0 = chain_id,
                            b1 = chain_id
                        )
                    },
                    control = list(
                        adapt_delta = 0.95,
                        max_treedepth = 15
                    )
                ),
            ),
            quiet = TRUE
        )
    })
    expect_snapshot(print(drawobj_b), cran = TRUE)

    impute_b <- impute(
        drawobj_b,
        references = c("TRT" = "TRT", "Placebo" = "Placebo"),
    )
    expect_snapshot(print(impute_b), cran = TRUE)

    v2 <- dobj$vars
    v2$covariates <- c("sex*age")
    analysis_b <- analyse(
        impute_b,
        fun = rbmi::ancova,
        delta = delta_template(impute_b),
        visits = c("visit_1", "visit_3"),
        vars = v2
    )
    expect_snapshot(print(analysis_b), cran = TRUE)
})





test_that("print - condmean bootstrap", {
    set.seed(313)
    dobj <- test_print_get_data(40)

    drawobj_cmb <- draws(
        data = dobj$dat,
        data_ice = dobj$dat_ice,
        vars = dobj$vars,
        method = method_condmean(
            n_samples = 0,     # Original dataset only (no samples)
            threshold = 0.2,
            type = "bootstrap",
            same_cov = TRUE,
            REML = TRUE,
            covariance = "ar1"   # Partial completion of argument name
        ),
        quiet = TRUE
    )
    expect_snapshot(print(drawobj_cmb), cran = TRUE)

    impute_cmb <- impute(
        drawobj_cmb,
        references = c("TRT" = "TRT", "Placebo" = "Placebo"),
    )
    expect_snapshot(print(impute_cmb), cran = TRUE)

    v2 <- dobj$vars
    v2$covariates <- c("sex")
    analysis_cmb <- analyse(
        impute_cmb,
        fun = ancova,
        vars = v2
    )
    expect_snapshot(print(analysis_cmb), cran = TRUE)

    ## Check that only point estimates are generated
    pool_ob <- pool(analysis_cmb)
    expect_true(all(!is.na(as.data.frame(pool_ob)$est)))
    expect_true(all(is.na(as.data.frame(pool_ob)$se)))
    expect_true(all(is.na(as.data.frame(pool_ob)$pval)))
})





test_that("print - condmean jackknife", {
    set.seed(89513)
    dobj <- test_print_get_data(35)
    drawobj_cmj <- draws(
        data = dobj$dat,
        data_ice = dobj$dat_ice,
        vars = dobj$vars,
        method = method_condmean(
            threshold = 0.5,
            same_cov = FALSE,
            REML = TRUE,
            type = "jackknife",
            covariance = "us"
        ),
        quiet = TRUE
    )
    expect_snapshot(print(drawobj_cmj), cran = TRUE)

    impute_cmj <- impute(
        drawobj_cmj,
        references = c("TRT" = "Placebo", "Placebo" = "Placebo"),
    )
    expect_snapshot(print(impute_cmj), cran = TRUE)


    v2 <- dobj$vars
    v2$covariates <- c("sex*age")
    analysis_cmj <- analyse(
        impute_cmj,
        fun = ancova,
        vars = v2
    )
    expect_snapshot(print(analysis_cmj), cran = TRUE)
})




test_that("print - bmlmi", {
    set.seed(2413)
    dobj <- test_print_get_data(40)

    drawobj_bml <- draws(
        ncores = 1,
        data = dobj$dat,
        data_ice = dobj$dat_ice,
        vars = dobj$vars,
        method = method_bmlmi(
            covariance = "cs",
            threshold = 0.05,
            same_cov = TRUE,
            REML = TRUE,
            B = 6,
            D = 4
        ),
        quiet = TRUE
    )
    expect_snapshot(print(drawobj_bml), cran = TRUE)

    impute_bml <- impute(
        drawobj_bml,
        references = c("TRT" = "Placebo", "Placebo" = "Placebo"),
    )
    expect_snapshot(print(impute_bml), cran = TRUE)

    compare_prop_lastvisit <- function(data, ...) {
        fit <- summary(
            glm(
                I(outcome > 10) ~ group,
                family = binomial(),
                data = data[data[["visit"]] == "visit_3", ]
            )
        )
        res <- list(
            trt = list(
                est = fit$coefficients["groupTRT", "Estimate"],
                se = fit$coefficients["groupTRT", "Std. Error"],
                df = Inf
            )
        )
        return(res)
    }
    analysis_bml <- analyse(
        impute_bml,
        fun = compare_prop_lastvisit
    )
    expect_snapshot(print(analysis_bml), cran = TRUE)
    expect_equal(analysis_bml$fun_name, "compare_prop_lastvisit")
})
