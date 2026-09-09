make_count_analysis_data <- function() {
    set.seed(4821)
    number_subjects <- 120
    periods <- 3
    subject <- factor(rep(seq_len(number_subjects), each = periods))
    group_by_subject <- factor(
        rep(c("Placebo", "Low", "High"), length.out = number_subjects),
        levels = c("Placebo", "Low", "High")
    )
    baseline_by_subject <- stats::rnorm(number_subjects)
    duration <- stats::runif(number_subjects * periods, 0.2, 0.5)
    linear_predictor <-
        -0.1 +
        0.3 * (group_by_subject == "Low") +
        0.6 * (group_by_subject == "High") +
        0.2 * baseline_by_subject
    frailty <- stats::rgamma(number_subjects, shape = 2, rate = 2)

    data.frame(
        subject = subject,
        period = rep(as.character(seq_len(periods)), number_subjects),
        duration = duration,
        count = stats::rpois(
            number_subjects * periods,
            lambda = duration * rep(exp(linear_predictor) * frailty, each = periods)
        ),
        group = rep(group_by_subject, each = periods),
        baseline = rep(baseline_by_subject, each = periods)
    )
}


test_that("negative binomial regression aggregates periods and returns coefficients", {
    withr::local_options(contrasts = c("contr.sum", "contr.poly"))
    data <- make_count_analysis_data()
    data_before <- data
    vars <- set_vars(
        subjid = "subject",
        period = "period",
        duration = "duration",
        outcome = "count",
        group = "group",
        covariates = "baseline"
    )

    result <- neg_bin_regression(data, vars)

    aggregate_data <- data.frame(
        count = as.numeric(rowsum(data$count, data$subject)),
        duration = as.numeric(rowsum(data$duration, data$subject)),
        group = data$group[!duplicated(data$subject)],
        baseline = data$baseline[!duplicated(data$subject)]
    )
    stats::contrasts(aggregate_data$group) <- stats::contr.treatment(
        levels(aggregate_data$group),
        base = 1
    )
    expected_model <- MASS::glm.nb(
        count ~ group + baseline + offset(log(duration)),
        data = aggregate_data,
        link = log,
        x = TRUE
    )
    expected_covariance <- glm_nb_covariance(expected_model)
    expected_coefficients <- names(stats::coef(expected_model))

    expect_named(result, c("(Intercept)", "groupLow", "groupHigh", "baseline"))
    expect_equal(
        unname(vapply(result, `[[`, numeric(1), "est")),
        unname(stats::coef(expected_model)[expected_coefficients])
    )
    expect_equal(
        unname(vapply(result, `[[`, numeric(1), "se")),
        unname(sqrt(diag(expected_covariance)[expected_coefficients]))
    )
    expect_true(all(is.infinite(vapply(result, `[[`, numeric(1), "df"))))
    expect_identical(data, data_before)
})


test_that("negative binomial regression accepts more than two treatment groups", {
    data <- make_count_analysis_data()
    vars <- set_vars(
        subjid = "subject",
        period = "period",
        duration = "duration",
        outcome = "count",
        group = "group"
    )

    result <- neg_bin_regression(data, vars)

    expect_length(result, 3)
    expect_named(result, c("(Intercept)", "groupLow", "groupHigh"))
})


test_that("negative binomial regression coefficients pool with normal inference", {
    data <- make_count_analysis_data()
    vars <- set_vars(
        subjid = "subject",
        period = "period",
        duration = "duration",
        outcome = "count",
        group = "group",
        covariates = "baseline"
    )
    analysis_results <- lapply(0:3, function(increment) {
        completed_data <- data
        completed_data$count[[1]] <- completed_data$count[[1]] + increment
        neg_bin_regression(completed_data, vars)
    })
    analysis <- as_analysis(
        results = analysis_results,
        method = method_bayes(n_samples = length(analysis_results)),
        outcome_type = "count"
    )

    observed <- pool(analysis)

    expect_named(
        observed$pars,
        c("(Intercept)", "groupLow", "groupHigh", "baseline")
    )
    for (coefficient in names(observed$pars)) {
        estimates <- vapply(
            analysis_results,
            function(result) result[[coefficient]]$est,
            numeric(1)
        )
        standard_errors <- vapply(
            analysis_results,
            function(result) result[[coefficient]]$se,
            numeric(1)
        )
        rubin <- rubin_rules(estimates, standard_errors, Inf)
        expected_se <- sqrt(rubin$var_t)

        expect_equal(observed$pars[[coefficient]]$est, rubin$est_point)
        expect_equal(observed$pars[[coefficient]]$se, expected_se)
        expect_equal(
            observed$pars[[coefficient]]$ci,
            rubin$est_point + c(-1, 1) * qnorm(0.975) * expected_se
        )
        expect_equal(
            observed$pars[[coefficient]]$pvalue,
            2 * pnorm(-abs(rubin$est_point / expected_se))
        )
    }
})


test_that("negative binomial regression validates count-analysis inputs", {
    data <- make_count_analysis_data()
    vars <- set_vars(
        subjid = "subject",
        period = "period",
        duration = "duration",
        outcome = "count",
        group = "group",
        covariates = "baseline"
    )

    data_non_integer <- data
    data_non_integer$count[[1]] <- 0.5
    expect_error(
        neg_bin_regression(data_non_integer, vars),
        "non-negative integer counts"
    )

    data_time_varying <- data
    data_time_varying$baseline[[2]] <- data_time_varying$baseline[[2]] + 1
    expect_error(
        neg_bin_regression(data_time_varying, vars),
        "constant within subject"
    )

    data_zero_duration <- data
    data_zero_duration$duration[data_zero_duration$subject == 1] <- 0
    expect_error(
        neg_bin_regression(data_zero_duration, vars),
        "greater than zero"
    )
})
