suppressPackageStartupMessages({
    library(testthat)
})


make_count_draws <- function(strategy = "JR", phi = 0.5) {
    dat <- data.frame(
        id = factor(
            rep(c("control", "active"), each = 3),
            levels = c("control", "active")
        ),
        period = rep(as.character(1:3), 2),
        duration = c(1, 1, 1, 1, 1, 1),
        outcome = c(2, 3, NA, 1, 2, NA),
        group = factor(
            rep(c("Control", "Active"), each = 3),
            levels = c("Control", "Active")
        )
    )
    vars <- set_vars(
        subjid = "id",
        period = "period",
        duration = "duration",
        outcome = "outcome",
        group = "group"
    )
    longdata <- longDataConstructor$new(dat, vars)
    longdata$set_strategies(data.frame(
        id = "active",
        strategy = strategy
    ))

    sample <- sample_single_count(
        ids = longdata$ids,
        beta = c(log(2), log(2)),
        phi = phi
    )
    result <- as_draws(
        method = method_bayes(n_samples = 1),
        samples = sample_list(sample),
        data = longdata,
        formula = longdata$formula,
        n_failures = 0
    )
    class(result) <- c("draws_count", class(result))
    result
}


test_that("count draws print endpoint-specific settings", {
    result <- make_count_draws()

    output <- capture.output(returned <- print(result))

    expect_identical(returned, result)
    expect_true(any(output == "Endpoint Type: count"))
    expect_true(any(output == "Imputation Type: random"))
    expect_true(any(output == "    same_cov: TRUE"))
    expect_false(any(grepl("covariance:", output, fixed = TRUE)))
    expect_false(any(grepl("prior_cov:", output, fixed = TRUE)))
    expect_false(any(grepl("init:", output, fixed = TRUE)))
})


test_that("count imputations print patients with positive duration by period", {
    draws <- make_count_draws()
    period_two_active <-
        draws$data$data$id == "active" & draws$data$data$period == "2"
    period_three_active <-
        draws$data$data$id == "active" & draws$data$data$period == "3"
    draws$data$data$duration[period_two_active] <- 0
    draws$data$data$outcome[period_two_active] <- NA
    draws$data$data$duration[period_three_active] <- 0
    draws$data$values$active[2] <- NA
    draws$data$is_missing$active[2] <- TRUE

    result <- impute(
        draws,
        c("Control" = "Control", "Active" = "Control")
    )
    output <- capture.output(returned <- print(result))

    expect_identical(returned, result)
    expect_true(any(output == "Endpoint Type: count"))
    expect_true(any(
        output == "Number of Patients with Positive Duration by Period:"
    ))
    expect_true(any(output == "    1: 2"))
    expect_true(any(output == "    2: 1"))
    expect_true(any(output == "    3: 1"))
    expect_false(any(grepl("Fraction of Missing Data", output, fixed = TRUE)))
})


test_that("count imputation samples the conditional negative binomial", {
    draws <- make_count_draws("JR")
    references <- c("Control" = "Control", "Active" = "Control")

    set.seed(731)
    expected <- stats::rnbinom(
        n = 2,
        size = c(2 + 5, 2 + 3),
        prob = c(
            (2 + 4) / (2 + 4 + 2),
            (2 + 8) / (2 + 8 + 2)
        )
    )
    set.seed(731)
    result <- impute(draws, references)
    completed <- extract_imputed_dfs(result)[[1]]

    expect_s3_class(result, "imputation_count")
    expect_equal(
        completed$outcome[completed$period == "3"],
        expected,
        tolerance = 0
    )
    expect_true(all(completed$outcome == trunc(completed$outcome)))
})


test_that("count imputation sequentially conditions multiple missing cells", {
    prepared <- list(
        id = rep(c("control", "active"), each = 4),
        subject_ids = c("control", "active"),
        subject_index = rep(1:2, each = 4),
        period = rep(as.character(1:4), 2),
        period_index = rep(1:4, 2),
        duration = rep(1, 8),
        outcome = c(2, 3, NA, NA, 1, 2, NA, NA),
        is_missing = rep(c(FALSE, FALSE, TRUE, TRUE), 2),
        own_group = rep(c("Control", "Active"), each = 4),
        reference_group = rep("Control", 8),
        strategy = rep("JR", 8),
        design_observed = cbind(
            intercept = 1,
            active = rep(c(0, 1), each = 4)
        ),
        design_missing = cbind(
            intercept = rep(1, 8),
            active = rep(0, 8)
        )
    )
    sample <- sample_single_count(
        ids = prepared$subject_ids,
        beta = c(log(2), log(2)),
        phi = 0.5
    )

    set.seed(840)
    expected <- numeric(8)
    for (subject in 1:2) {
        rows <- (subject - 1) * 4 + 3:4
        size <- 2 + c(5, 3)[subject]
        mass <- 2 + c(4, 8)[subject]
        for (row in rows) {
            expected[row] <- stats::rnbinom(
                1,
                size = size,
                prob = mass / (mass + 2)
            )
            size <- size + expected[row]
            mass <- mass + 2
        }
    }
    set.seed(840)
    actual <- sample_count_outcomes(prepared, sample)

    expect_identical(actual, expected)
})


test_that("count imputation requires missing cells in period order", {
    draws <- make_count_draws("MAR")
    prepared <- prepare_count_imputation_data(
        data = draws$data,
        references = add_class(
            c("Control" = "Control", "Active" = "Active"),
            "references"
        ),
        strategy_by_id = unlist(draws$data$strategies)
    )
    active_rows <- which(prepared$id == "active")
    prepared$is_missing[active_rows[2]] <- TRUE
    prepared$outcome[active_rows[2]] <- NA
    prepared$period_index[active_rows[2:3]] <- c(3, 2)

    expect_error(
        sample_count_outcomes(prepared, draws$samples[[1]]),
        "missing cells must be ordered by period"
    )
})


test_that("CR uses reference means for the conditioning periods", {
    draws <- make_count_draws("CR")
    references <- c("Control" = "Control", "Active" = "Control")
    prepared <- prepare_count_imputation_data(
        data = draws$data,
        references = add_class(references, "references"),
        strategy_by_id = unlist(draws$data$strategies)
    )

    set.seed(902)
    expected <- stats::rnbinom(
        n = 2,
        size = c(2 + 5, 2 + 3),
        prob = c(
            (2 + 4) / (2 + 4 + 2),
            (2 + 4) / (2 + 4 + 2)
        )
    )
    set.seed(902)
    actual <- sample_count_outcomes(prepared, draws$samples[[1]])

    expect_equal(actual[prepared$period == "3"], expected)
})


test_that("MAR uses the subject arm dispersion parameter", {
    draws <- make_count_draws(
        strategy = "MAR",
        phi = c("Control" = 0.5, "Active" = 0.1)
    )

    set.seed(903)
    expected <- stats::rnbinom(
        n = 2,
        size = c(2 + 5, 10 + 3),
        prob = c(
            (2 + 4) / (2 + 4 + 2),
            (10 + 8) / (10 + 8 + 4)
        )
    )
    set.seed(903)
    actual <- impute(draws)
    completed <- extract_imputed_dfs(actual)[[1]]

    expect_equal(completed$outcome[completed$period == "3"], expected)
})


test_that("JR and CR use the mapped reference arm dispersion parameter", {
    references <- c("Control" = "Control", "Active" = "Control")
    group_phi <- c("Control" = 0.5, "Active" = 0.1)

    for (strategy in c("JR", "CR")) {
        draws <- make_count_draws(strategy = strategy, phi = group_phi)
        prepared <- prepare_count_imputation_data(
            data = draws$data,
            references = add_class(references, "references"),
            strategy_by_id = unlist(draws$data$strategies)
        )

        observed_mu_active <- ife(strategy == "CR", 4, 8)
        set.seed(5)
        expected <- stats::rnbinom(
            n = 2,
            size = c(2 + 5, 2 + 3),
            prob = c(
                (2 + 4) / (2 + 4 + 2),
                (2 + observed_mu_active) /
                    (2 + observed_mu_active + 2)
            )
        )
        set.seed(5)
        actual <- sample_count_outcomes(prepared, draws$samples[[1]])

        expect_equal(actual[prepared$period == "3"], expected)
    }
})


test_that("count rate multiplier adjusts only selected subjects and periods", {
    draws <- make_count_draws(strategy = "MAR", phi = 0.5)
    updated_strategy <- data.frame(
        id = "active",
        strategy = "DOUBLE_RATE"
    )
    strategies <- get_count_strategies(
        DOUBLE_RATE = count_strategy(
            base = "MAR",
            rate_multiplier = 2,
            period = "3"
        )
    )

    set.seed(904)
    expected <- stats::rnbinom(
        n = 2,
        size = c(2 + 5, 2 + 3),
        prob = c(
            (2 + 4) / (2 + 4 + 2),
            (2 + 8) / (2 + 8 + 2 * 4)
        )
    )
    set.seed(904)
    actual <- impute(
        draws,
        update_strategy = updated_strategy,
        strategies = strategies
    )
    completed <- extract_imputed_dfs(actual)[[1]]

    expect_equal(completed$outcome[completed$period == "3"], expected)
    expect_identical(draws$data$strategies$active, "MAR")
    expect_identical(actual$data$strategies$active, "DOUBLE_RATE")
    expect_s3_class(
        actual$count_strategies$DOUBLE_RATE,
        "count_strategy"
    )
})


test_that("fixed lambda rate uses the posterior dispersion draw", {
    draws <- make_count_draws(strategy = "MAR", phi = 0.5)
    updated_strategy <- data.frame(
        id = "active",
        strategy = "FIXED_LAMBDA"
    )
    strategies <- get_count_strategies(
        FIXED_LAMBDA = count_strategy(
            base = "MAR",
            fixed_lambda_rate = 0.25,
            period = "3"
        )
    )

    set.seed(905)
    expected <- stats::rnbinom(
        n = 2,
        size = c(2 + 5, 2 + 3),
        prob = c(
            (2 + 4) / (2 + 4 + 2),
            (2 + 8) / (2 + 8 + 0.25 / 0.5)
        )
    )
    set.seed(905)
    actual <- impute(
        draws,
        update_strategy = updated_strategy,
        strategies = strategies
    )
    completed <- extract_imputed_dfs(actual)[[1]]

    expect_equal(completed$outcome[completed$period == "3"], expected)
})


test_that("controlled count strategy inputs are validated", {
    expect_error(
        count_strategy(rate_multiplier = -1),
        "Invalid"
    )
    expect_error(
        count_strategy(
            rate_multiplier = 2,
            fixed_lambda_rate = 0.1
        ),
        "Only one"
    )
    expect_error(
        get_count_strategies(count_strategy()),
        "must be named"
    )
})

test_that("controlled count strategy periods must occur in the data", {
    draws <- make_count_draws(strategy = "MAR")
    strategies <- get_count_strategies(
        INVALID_PERIOD = count_strategy(
            base = "MAR",
            rate_multiplier = 2,
            period = "99"
        )
    )

    expect_error(
        impute(
            draws,
            update_strategy = data.frame(
                id = "active",
                strategy = "INVALID_PERIOD"
            ),
            strategies = strategies
        ),
        "Count strategy periods not found in the data: 99"
    )
})

test_that("group-specific dispersion draws must be named", {
    expect_error(
        sample_single_count(
            ids = c("control", "active"),
            beta = c(0, 0),
            phi = c(0.5, 0.1)
        )
    )
})


test_that("zero-duration missing count cells are completed with zero", {
    draws <- make_count_draws("JR")
    period_two_active <-
        draws$data$data$id == "active" & draws$data$data$period == "2"
    draws$data$data$duration[period_two_active] <- 0
    draws$data$data$outcome[period_two_active] <- NA
    draws$data$values$active[2] <- NA
    draws$data$is_missing$active[2] <- TRUE

    result <- impute(
        draws,
        c("Control" = "Control", "Active" = "Control")
    )
    completed <- extract_imputed_dfs(result)[[1]]

    expect_equal(completed$outcome[completed$duration == 0], 0)
})


test_that("count imputation validates references and supported strategies", {
    expect_error(
        impute(make_count_draws("JR")),
        "specify the references"
    )
    expect_error(
        impute(
            make_count_draws("CIR"),
            c("Control" = "Control", "Active" = "Control")
        ),
        "currently support"
    )
})


test_that("non-MAR count strategies retain all observed draw-model cells", {
    draws <- make_count_draws("CR")
    actual <- extract_data_mnar_as_na(draws$data)

    expect_equal(nrow(actual), 6)
    expect_false(anyNA(actual$outcome[actual$period %in% c("1", "2")]))
})
