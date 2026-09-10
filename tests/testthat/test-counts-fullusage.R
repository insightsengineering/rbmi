suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
})


make_count_fullusage_data <- function(
    n = 120,
    periods = as.character(1:3),
    prob_miss = c(0, 0, 0.45),
    phi = 0.35
) {
    groups <- c("Placebo", "Low", "High")
    group_effect <- c(0, log(1.4), log(1.9))
    period_effect <- seq(0.2, -0.1, length.out = length(periods))
    log_rates <- outer(group_effect, period_effect, "+")
    rownames(log_rates) <- groups

    dat <- simulate_count_data(
        n = n,
        groups = groups,
        periods = periods,
        log_rates = log_rates,
        duration = seq(0.8, 1.2, length.out = length(periods)),
        phi = phi,
        baseline_effect = 0.2,
        prob_miss = prob_miss
    )
    dat$OnOff <- factor(
        ifelse(as.integer(dat$period) == 1, "On", "Off"),
        levels = c("On", "Off")
    )
    dat
}


make_count_ice <- function(data, strategy = "MAR") {
    data |>
        filter(is.na(outcome), duration > 0) |>
        distinct(id) |>
        mutate(strategy = strategy)
}


count_fullusage_method <- function(same_cov = TRUE, seed = 8192) {
    method_bayes(
        same_cov = same_cov,
        n_samples = 50,
        control = control_bayes(
            warmup = 50,
            thin = 1,
            chains = 1,
            seed = seed,
            control = list(adapt_delta = 0.9)
        )
    )
}


run_count_fullusage <- function(
    data,
    data_ice,
    draw_vars,
    analysis_vars,
    method,
    update_strategy = NULL,
    strategies = get_count_strategies()
) {
    draw_object <- suppressWarnings(draws(
        outcome = "count",
        data = data,
        data_ice = data_ice,
        vars = draw_vars,
        method = method,
        quiet = TRUE
    ))
    set.seed(9751)
    imputation_object <- impute(
        draw_object,
        update_strategy = update_strategy,
        strategies = strategies
    )
    analysis_object <- analyse(
        imputation_object,
        neg_bin_regression,
        vars = analysis_vars
    )
    pool_object <- pool(
        analysis_object,
        conf.level = 0.99,
        alternative = "two.sided"
    )

    expect_s3_class(draw_object, "draws_count")
    expect_s3_class(imputation_object, "imputation_count")
    expect_s3_class(analysis_object, "analysis")
    expect_s3_class(pool_object, "pool")
    completed <- extract_imputed_dfs(imputation_object)
    expect_length(completed, method$n_samples)
    expect_true(all(vapply(
        completed,
        function(x) !anyNA(x[[analysis_vars$outcome]]),
        logical(1)
    )))

    list(
        draws = draw_object,
        imputations = imputation_object,
        analysis = analysis_object,
        pooled = pool_object
    )
}


expect_count_treatment_order <- function(pool_object) {
    expect_named(
        pool_object$pars,
        c("(Intercept)", "groupLow", "groupHigh", "baseline")
    )
    expect_gt(pool_object$pars$groupLow$est, 0)
    expect_gt(pool_object$pars$groupHigh$est, pool_object$pars$groupLow$est)
    expect_contains(pool_object$pars$groupLow$ci, log(1.4))
    expect_contains(pool_object$pars$groupHigh$ci, log(1.9))

    displayed <- as.data.frame(pool_object)
    expect_true(all(is.finite(unlist(
        displayed[, c("est", "se", "lci", "uci", "pval")]
    ))))
    expect_true(all(displayed$est > 0))
    expect_true(all(displayed$lci < displayed$est))
    expect_true(all(displayed$est < displayed$uci))
}


count_numeric_summary <- function(pool_object) {
    parameters <- c("groupLow", "groupHigh", "baseline")
    result <- t(vapply(pool_object$pars[parameters], function(parameter) {
        c(
            estimate = parameter$est,
            standard_error = parameter$se,
            lower = parameter$ci[[1]],
            upper = parameter$ci[[2]],
            p_value = parameter$pvalue
        )
    }, numeric(5)))
    result
}


test_that("simulate_count_data generates correlated count trial data", {
    set.seed(712)
    log_rates <- rbind(
        Control = log(c(1, 2, 3)),
        Active = log(c(2, 4, 6))
    )
    observed <- simulate_count_data(
        n = c(7, 9),
        groups = rownames(log_rates),
        periods = as.character(1:3),
        log_rates = log_rates,
        duration = c(0.5, 1, 2),
        phi = c(0.2, 0.8),
        baseline_effect = 0.1,
        prob_miss = c(0, 0, 1)
    )

    expect_s3_class(observed, "data.frame")
    expect_named(
        observed,
        c(
            "id", "period", "duration", "group", "baseline",
            "outcome_full", "outcome"
        )
    )
    expect_equal(nrow(observed), (7 + 9) * 3)
    expect_length(unique(observed$id), 16)
    expect_identical(levels(observed$group), c("Control", "Active"))
    expect_identical(levels(observed$period), as.character(1:3))
    expect_true(all(observed$outcome_full >= 0))
    expect_true(all(observed$outcome_full == trunc(observed$outcome_full)))
    expect_false(anyNA(observed$outcome[observed$period != "3"]))
    expect_true(all(is.na(observed$outcome[observed$period == "3"])))

    expect_error(simulate_count_data(n = 0), "positive integers")
    expect_error(simulate_count_data(phi = -1), "positive values")
    expect_error(simulate_count_data(prob_miss = 2), "probabilities")
})


test_that("full count usage follows all models in the count vignette", {
    skip_if_not(is_extended_test())

    set.seed(4681)
    dat <- make_count_fullusage_data()
    dat_ice <- make_count_ice(dat)
    analysis_vars <- set_vars(
        outcome = "outcome",
        subjid = "id",
        period = "period",
        duration = "duration",
        group = "group",
        covariates = "baseline"
    )

    # Main vignette model: treatment by on/off-treatment status and baseline.
    shared_vars <- set_vars(
        outcome = "outcome",
        subjid = "id",
        period = "period",
        duration = "duration",
        group = "group",
        covariates = c("group * OnOff", "baseline")
    )
    shared <- run_count_fullusage(
        dat,
        dat_ice,
        shared_vars,
        analysis_vars,
        count_fullusage_method(seed = 1001)
    )
    expect_count_treatment_order(shared$pooled)
    expect_snapshot_value(
        count_numeric_summary(shared$pooled),
        style = "deparse",
        tolerance = 0.02
    )
    expect_true(all(vapply(
        shared$draws$samples,
        function(x) length(x$phi) == 1,
        logical(1)
    )))

    # Fully treatment-interacted fixed effects, first with shared phi and then
    # with the separate-by-arm phi model shown immediately before it.
    interacted_vars <- set_vars(
        outcome = "outcome",
        subjid = "id",
        period = "period",
        duration = "duration",
        group = "group",
        covariates = c("group:OnOff", "group:baseline")
    )
    interacted_shared <- run_count_fullusage(
        dat,
        dat_ice,
        interacted_vars,
        analysis_vars,
        count_fullusage_method(seed = 1002)
    )
    expect_count_treatment_order(interacted_shared$pooled)
    expect_snapshot_value(
        count_numeric_summary(interacted_shared$pooled),
        style = "deparse",
        tolerance = 0.02
    )

    interacted_by_arm <- run_count_fullusage(
        dat,
        dat_ice,
        interacted_vars,
        analysis_vars,
        count_fullusage_method(same_cov = FALSE, seed = 1003)
    )
    expect_count_treatment_order(interacted_by_arm$pooled)
    expect_snapshot_value(
        count_numeric_summary(interacted_by_arm$pooled),
        style = "deparse",
        tolerance = 0.02
    )
    expect_true(all(vapply(
        interacted_by_arm$draws$samples,
        function(x) identical(names(x$phi), levels(dat$group)),
        logical(1)
    )))

    # Controlled imputations reuse the main posterior draws, as in the
    # vignette, rather than fitting the observed-data model again.
    active_missing <- dat |>
        filter(group != "Placebo", is.na(outcome)) |>
        distinct(id)
    fixed_update <- mutate(active_missing, strategy = "FIXED_LAMBDA")
    double_update <- mutate(active_missing, strategy = "DOUBLE_RATE")

    set.seed(9751)
    fixed_imputations <- impute(
        shared$draws,
        update_strategy = fixed_update,
        strategies = get_count_strategies(
            FIXED_LAMBDA = count_strategy(
                base = "MAR",
                fixed_lambda_rate = 0.15,
                period = "3"
            )
        )
    )
    fixed_analysis <- analyse(
        fixed_imputations,
        neg_bin_regression,
        vars = analysis_vars
    )
    fixed_pool <- pool(fixed_analysis)
    expect_s3_class(fixed_pool, "pool")
    expect_true(all(is.finite(vapply(
        fixed_pool$pars,
        `[[`,
        numeric(1),
        "est"
    ))))

    set.seed(9751)
    doubled_imputations <- impute(
        shared$draws,
        update_strategy = double_update,
        strategies = get_count_strategies(
            DOUBLE_RATE = count_strategy(
                base = "MAR",
                rate_multiplier = 2,
                period = "3"
            )
        )
    )
    mean_active_imputed <- function(x) {
        mean(vapply(extract_imputed_dfs(x), function(completed) {
            mean(completed$outcome[
                dat$group != "Placebo" & is.na(dat$outcome)
            ])
        }, numeric(1)))
    }
    controlled_imputation_means <- c(
        mar = mean_active_imputed(shared$imputations),
        fixed_lambda = mean_active_imputed(fixed_imputations),
        double_rate = mean_active_imputed(doubled_imputations)
    )
    expect_gt(
        controlled_imputation_means[["double_rate"]],
        controlled_imputation_means[["mar"]]
    )

    doubled_analysis <- analyse(
        doubled_imputations,
        neg_bin_regression,
        vars = analysis_vars
    )
    doubled_pool <- pool(doubled_analysis)
    expect_s3_class(doubled_pool, "pool")
    expect_snapshot_value(
        list(
            imputed_means = controlled_imputation_means,
            fixed_lambda = count_numeric_summary(fixed_pool),
            double_rate = count_numeric_summary(doubled_pool)
        ),
        style = "deparse",
        tolerance = 0.02
    )

    # Seasonal/ragged model: subjects have different cell sets and some have
    # two missing cells, exercising sequential count imputation.
    set.seed(9321)
    seasonal <- make_count_fullusage_data(
        n = 80,
        periods = as.character(1:6),
        prob_miss = c(0, 0, 0, 0.15, 0.35, 0.5)
    )
    seasonal$Quarter <- factor(
        1 + (as.integer(seasonal$period) - 1) %% 4,
        levels = 1:4
    )
    subject_number <- as.integer(sub("id_", "", as.character(seasonal$id)))
    keep <- !(subject_number %% 4 == 0 & seasonal$period %in% c("2", "5"))
    seasonal <- droplevels(seasonal[keep, ])
    expect_gt(length(unique(table(seasonal$id))), 1)
    expect_true(any(table(seasonal$id[is.na(seasonal$outcome)]) > 1))

    seasonal_ice <- make_count_ice(seasonal)
    seasonal_vars <- set_vars(
        outcome = "outcome",
        subjid = "id",
        period = "period",
        duration = "duration",
        group = "group",
        covariates = c("group:OnOff", "baseline", "Quarter")
    )
    seasonal_analysis_vars <- set_vars(
        outcome = "outcome",
        subjid = "id",
        period = "period",
        duration = "duration",
        group = "group",
        covariates = "baseline"
    )
    seasonal_result <- run_count_fullusage(
        seasonal,
        seasonal_ice,
        seasonal_vars,
        seasonal_analysis_vars,
        count_fullusage_method(seed = 1004)
    )
    expect_count_treatment_order(seasonal_result$pooled)
    expect_snapshot_value(
        count_numeric_summary(seasonal_result$pooled),
        style = "deparse",
        tolerance = 0.02
    )
})
