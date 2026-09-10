#' Define controlled imputation strategies for count outcomes
#'
#' `count_strategy()` defines how the model-based mean for a missing count is
#' obtained and optionally adjusted during imputation. The `base` strategy is
#' applied first. A `rate_multiplier` then multiplies its model-based mean, or
#' `fixed_lambda_rate` replaces that mean using the negative-multinomial lambda
#' parameterisation described below.
#'
#' `get_count_strategies()` returns the built-in MAR, JR and CR count strategies
#' together with any named user-defined strategies supplied through `...`.
#' For count outcomes, the strategy is assigned at the subject level and applies
#' to every missing positive-duration period for that subject. No ICE time is
#' supplied or inferred; consequently, JR and CR are not restricted to periods
#' after an ICE.
#'
#' @param base Character. The base count imputation strategy, one of `"MAR"`,
#'   `"JR"`, or `"CR"`.
#' @param rate_multiplier A non-negative numeric scalar multiplying the
#'   model-based mean for affected missing cells.
#' @param fixed_lambda_rate `NULL` or a non-negative numeric scalar. When set,
#'   it replaces the model-based rate on the SAS negative-multinomial lambda
#'   scale. For exposure `L` and the strategy-selected dispersion `phi`, the
#'   Poisson mean supplied to the negative-binomial imputation is
#'   `L * fixed_lambda_rate / phi`. MAR selects `phi` from the subject's arm;
#'   JR and CR select it from the mapped reference arm. This is the unconditional
#'   marginal mean for that cell in the joint count model; the mean of the
#'   imputation distribution conditional on the other observed cells also
#'   depends on their counts and fitted means.
#' @param period `NULL` or a vector of period values. If supplied, the rate
#'   adjustment is restricted to missing cells in these periods. The base
#'   strategy still applies to other missing periods.
#' @param ... Named `count_strategy` objects to add to the built-in strategies.
#'
#' @return `count_strategy()` returns a `count_strategy` object.
#'   `get_count_strategies()` returns a named list of these objects for use in
#'   the `strategies` argument of [impute()].
#'
#' @examples
#' fixed_lambda <- count_strategy(
#'     base = "MAR",
#'     fixed_lambda_rate = 0.0021,
#'     period = "3"
#' )
#' double_rate <- count_strategy(
#'     base = "MAR",
#'     rate_multiplier = 2,
#'     period = "3"
#' )
#' get_count_strategies(
#'     FIXED_LAMBDA = fixed_lambda,
#'     DOUBLE_RATE = double_rate
#' )
#'
#' @export
count_strategy <- function(
    base = c("MAR", "JR", "CR"),
    rate_multiplier = 1,
    fixed_lambda_rate = NULL,
    period = NULL
) {
    base <- match.arg(base)
    fixed_lambda_rate <- ife(
        is.null(fixed_lambda_rate),
        NA_real_,
        fixed_lambda_rate
    )
    if (!is.null(period)) {
        period <- unique(as.character(period))
    }

    x <- list(
        base = base,
        rate_multiplier = rate_multiplier,
        fixed_lambda_rate = fixed_lambda_rate,
        period = period
    )
    class(x) <- c("count_strategy", "list")
    validate_count_strategy(x)
    x
}


#' @rdname count_strategy
#' @export
get_count_strategies <- function(...) {
    user_strategies <- list(...)
    user_names <- names(user_strategies)
    assert_that(
        length(user_strategies) == 0 ||
            (!is.null(user_names) && all(nzchar(user_names))),
        msg = "User-defined count strategies must be named"
    )
    assert_that(
        !anyDuplicated(user_names),
        msg = "Count strategies must be uniquely named"
    )

    strategies <- list(
        MAR = count_strategy("MAR"),
        JR = count_strategy("JR"),
        CR = count_strategy("CR")
    )
    for (strategy_name in user_names) {
        validate_count_strategy(user_strategies[[strategy_name]])
        strategies[[strategy_name]] <- user_strategies[[strategy_name]]
    }
    strategies
}


#' Validate a controlled count imputation strategy
#'
#' @param x An object created by [count_strategy()].
#'
#' @return `TRUE` invisibly, or an error for an invalid strategy.
#'
#' @keywords internal
validate_count_strategy <- function(x) {
    assert_that(
        has_class(x, "count_strategy"),
        is.list(x),
        identical(
            names(x),
            c(
                "base",
                "rate_multiplier",
                "fixed_lambda_rate",
                "period"
            )
        ),
        x$base %in% c("MAR", "JR", "CR"),
        length(x$base) == 1,
        is.numeric(x$rate_multiplier),
        length(x$rate_multiplier) == 1,
        is.finite(x$rate_multiplier),
        x$rate_multiplier >= 0,
        is.numeric(x$fixed_lambda_rate),
        length(x$fixed_lambda_rate) == 1,
        is.na(x$fixed_lambda_rate) ||
            (is.finite(x$fixed_lambda_rate) && x$fixed_lambda_rate >= 0),
        is.null(x$period) ||
            (is.character(x$period) &&
                length(x$period) >= 1 &&
                all(!is.na(x$period)) &&
                all(nzchar(x$period))),
        msg = "Invalid `count_strategy` object"
    )
    assert_that(
        is.na(x$fixed_lambda_rate) || x$rate_multiplier == 1,
        msg = paste(
            "Only one of a non-default `rate_multiplier` and",
            "`fixed_lambda_rate` can be specified"
        )
    )
    invisible(TRUE)
}


#' Resolve count imputation strategy definitions
#'
#' @param strategies A named list containing count strategies.
#' @param reference Character vector of strategy names required by the data.
#'
#' @return A named list containing the required validated count strategies.
#'
#' @keywords internal
resolve_count_strategies <- function(strategies, reference) {
    assert_that(
        is.list(strategies),
        !is.null(names(strategies)),
        !anyDuplicated(names(strategies)),
        msg = "`strategies` must be a uniquely named list"
    )
    required <- unique(unname(reference))
    missing_strategies <- setdiff(required, names(strategies))
    assert_that(
        length(missing_strategies) == 0,
        msg = sprintf(
            paste(
                "Count outcomes currently support strategies with a count",
                "implementation; none is available for %s"
            ),
            paste0("`", missing_strategies, "`", collapse = ", ")
        )
    )

    resolved <- lapply(required, function(strategy_name) {
        strategy <- strategies[[strategy_name]]
        # Retain compatibility with getStrategies() for the three count
        # strategies that were supported before controlled count strategies.
        if (is.function(strategy) && strategy_name %in% c("MAR", "JR", "CR")) {
            strategy <- count_strategy(strategy_name)
        }
        validate_count_strategy(strategy)
        strategy
    })
    names(resolved) <- required
    resolved
}


#' Apply strategy updates locally to count imputation data
#'
#' @param data A cloned `longdata` object for a count endpoint.
#' @param update_strategy A data frame accepted by the `update_strategy`
#'   argument of [impute()].
#'
#' @return The modified `longdata` object.
#'
#' @keywords internal
update_count_strategies <- function(data, update_strategy) {
    if (is.null(update_strategy)) {
        return(data)
    }
    update_strategy <- as_dataframe(update_strategy)
    validate_dataice(
        data = data$data,
        data_ice = update_strategy,
        vars = data$vars,
        update = TRUE
    )
    id_var <- data$vars$subjid
    strategy_var <- data$vars$strategy
    for (row in seq_len(nrow(update_strategy))) {
        id <- as.character(update_strategy[[id_var]][row])
        data$strategies[[id]] <- update_strategy[[strategy_var]][row]
    }
    data
}


#' Impute missing count outcomes
#'
#' For count outcomes, missing positive-duration cells are sampled sequentially
#' from their conditional negative binomial distributions given the observed
#' cells and any previously imputed cells for the same subject. Missing cells
#' are visited in period-factor level order. The model uses the dispersion
#' parameterisation `Var(Y) = mu + phi * mu^2`.
#'
#' @rdname impute
#' @export
impute.draws_count <- function(
    draws,
    references = NULL,
    update_strategy = NULL,
    strategies = get_count_strategies()
) {
    validate(draws)
    data <- draws$data$clone(deep = TRUE)
    data <- update_count_strategies(data, update_strategy)
    strategy_by_id <- unlist(data$strategies, use.names = TRUE)
    resolved_strategies <- resolve_count_strategies(
        strategies = strategies,
        reference = strategy_by_id
    )
    strategy_specs_by_id <- lapply(
        strategy_by_id,
        function(strategy_name) resolved_strategies[[strategy_name]]
    )
    names(strategy_specs_by_id) <- names(strategy_by_id)
    base_strategy_by_id <- vapply(
        strategy_specs_by_id,
        `[[`,
        character(1),
        "base"
    )

    if (is.null(references)) {
        assert_that(
            all(base_strategy_by_id == "MAR"),
            msg = paste(
                "You have set a non-MAR imputation strategy.",
                "Please specify the references using the argument `references`"
            )
        )
        references <- levels(data$data[[data$vars$group]])
        names(references) <- references
    }
    references <- add_class(references, "references")
    validate(references, data$data[[data$vars$group]])

    groups_by_id <- vapply(data$group, as.character, character(1))
    assert_that(
        all(groups_by_id %in% names(references)),
        msg = "`references` must provide a mapping for every treatment group"
    )

    prepared <- prepare_count_imputation_data(
        data = data,
        references = references,
        strategy_by_id = base_strategy_by_id,
        strategy_specs_by_id = strategy_specs_by_id
    )

    imputations <- lapply(draws$samples, function(sample) {
        imputed_values <- sample_count_outcomes(prepared, sample)
        imputation_df(lapply(sample$ids, function(id) {
            subject_rows <- prepared$id == id
            imputation_single(
                id = id,
                values = imputed_values[subject_rows & prepared$is_missing]
            )
        }))
    })
    imputations <- do.call(imputation_list_df, imputations)

    result <- as_imputation(
        imputations = imputations,
        data = data,
        method = draws$method,
        references = references
    )
    result$count_strategies <- resolved_strategies
    class(result) <- c("imputation_count", class(result))
    validate(result)
    result
}


#' @describeIn print.imputation Print the number of patients with positive
#'   duration in each period for count endpoints. Zero-duration rows are
#'   structural placeholders and are not counted.
#' @export
print.imputation_count <- function(x, ...) {
    data <- x$data$data
    vars <- x$data$vars
    periods <- x$data$periods
    period <- as.character(data[[vars$period]])
    duration <- data[[vars$duration]]
    subjid <- data[[vars$subjid]]

    n_patients <- vapply(
        periods,
        function(current_period) {
            rows <- period == current_period & duration > 0
            length(unique(subjid[rows]))
        },
        integer(1)
    )
    width <- max(nchar(periods))
    period_strings <- sprintf(
        paste0("%-", width, "s: %s"),
        periods,
        n_patients
    )

    ref_from <- names(x$references)
    ref_to <- x$references
    width <- max(nchar(ref_from))
    ref_strings <- sprintf(
        paste0("%-", width, "s -> %s"),
        ref_from,
        ref_to
    )

    n_imp <- length(x$imputations)
    n_imp_string <- ife(
        has_class(x$method, "condmean"),
        sprintf("1 + %s", n_imp - 1),
        as.character(n_imp)
    )

    string <- c(
        "",
        "Imputation Object",
        "-----------------",
        sprintf("Number of Imputed Datasets: %s", n_imp_string),
        "Endpoint Type: count",
        "Number of Patients with Positive Duration by Period:",
        sprintf("    %s", period_strings),
        "References:",
        sprintf("    %s", ref_strings),
        ""
    )

    cat(string, sep = "\n")
    return(invisible(x))
}


#' Prepare fixed inputs for conditional count imputation
#'
#' @param data A `longdata` object containing the original count data and model
#'   formula.
#' @param references A validated named reference-group mapping.
#' @param strategy_by_id A named character vector containing one imputation
#'   strategy per subject.
#' @param strategy_specs_by_id `NULL` or a named list of `count_strategy`
#'   objects containing the controlled-imputation settings for each subject.
#'
#' @return A list of row-aligned observed and missing design matrices, outcome
#'   and duration values, subject indices, treatment groups, references, and
#'   imputation strategies used by [sample_count_outcomes()].
#'
#' @keywords internal
prepare_count_imputation_data <- function(
    data,
    references,
    strategy_by_id,
    strategy_specs_by_id = NULL
) {
    vars <- data$vars
    dat <- data$data
    id <- as.character(dat[[vars$subjid]])
    period <- as.character(dat[[vars$period]])
    duration <- dat[[vars$duration]]
    outcome <- dat[[vars$outcome]]
    is_missing <- is.na(outcome)

    subject_index <- match(id, data$ids)
    assert_that(!anyNA(subject_index))

    design_own <- as.matrix(as_model_df(dat, data$formula)[, -1, drop = FALSE])
    dat_reference <- dat
    reference_group <- unname(references[as.character(dat[[vars$group]])])
    own_group <- as.character(dat[[vars$group]])
    dat_reference[[vars$group]] <- factor(
        reference_group,
        levels = levels(dat[[vars$group]])
    )
    design_reference <- as.matrix(
        as_model_df(dat_reference, data$formula)[, -1, drop = FALSE]
    )

    strategy <- unname(strategy_by_id[id])
    if (is.null(strategy_specs_by_id)) {
        strategy_specs_by_id <- lapply(
            strategy_by_id,
            function(base) count_strategy(base = base)
        )
        names(strategy_specs_by_id) <- names(strategy_by_id)
    }
    assert_that(
        all(data$ids %in% names(strategy_specs_by_id)),
        msg = "Count strategy specifications must cover every subject"
    )
    specified_periods <- unique(unlist(lapply(
        strategy_specs_by_id[data$ids],
        `[[`,
        "period"
    )))
    unmatched_periods <- setdiff(specified_periods, as.character(data$periods))
    assert_that(
        !length(unmatched_periods),
        msg = paste(
            "Count strategy periods not found in the data:",
            paste(unmatched_periods, collapse = ", ")
        )
    )
    rate_multiplier <- rep(1, length(id))
    fixed_lambda_rate <- rep(NA_real_, length(id))
    for (subject_id in data$ids) {
        strategy_spec <- strategy_specs_by_id[[subject_id]]
        validate_count_strategy(strategy_spec)
        subject_rows <- id == subject_id
        affected_rows <- subject_rows &
            (is.null(strategy_spec$period) |
                period %in% strategy_spec$period)
        rate_multiplier[affected_rows] <- strategy_spec$rate_multiplier
        fixed_lambda_rate[affected_rows] <-
            strategy_spec$fixed_lambda_rate
    }
    use_reference_observed <- strategy == "CR" & !is_missing
    use_reference_missing <- strategy %in% c("JR", "CR") & is_missing
    design_observed <- design_own
    design_missing <- design_own
    design_observed[use_reference_observed, ] <-
        design_reference[use_reference_observed, , drop = FALSE]
    design_missing[use_reference_missing, ] <-
        design_reference[use_reference_missing, , drop = FALSE]

    list(
        id = id,
        subject_ids = data$ids,
        subject_index = subject_index,
        period = period,
        period_index = match(period, data$periods),
        duration = duration,
        outcome = outcome,
        is_missing = is_missing,
        own_group = own_group,
        reference_group = reference_group,
        strategy = strategy,
        rate_multiplier = rate_multiplier,
        fixed_lambda_rate = fixed_lambda_rate,
        design_observed = design_observed,
        design_missing = design_missing
    )
}


#' Sample missing counts for one posterior parameter draw
#'
#' @param prepared The fixed imputation inputs returned by
#'   [prepare_count_imputation_data()].
#' @param sample A `sample_single_count` posterior draw containing regression
#'   coefficients and dispersion parameters.
#'
#' @return A numeric vector aligned with the rows of `prepared`. Observed rows
#'   contain zero, zero-duration missing rows contain zero, and
#'   positive-duration missing rows contain the sampled counts.
#'
#' @keywords internal
sample_count_outcomes <- function(prepared, sample) {
    validate(sample)
    assert_that(
        length(sample$beta) == ncol(prepared$design_observed),
        msg = "The count-model coefficient draw is incompatible with the design matrix"
    )

    observed_available <- !prepared$is_missing & prepared$duration > 0
    needs_random_draw <- prepared$is_missing & prepared$duration > 0
    missing_period_index <- prepared$period_index[needs_random_draw]
    period_index_by_subject <- split(
        missing_period_index,
        prepared$subject_index[needs_random_draw]
    )
    assert_that(
        length(prepared$period_index) == length(prepared$id),
        !anyNA(missing_period_index),
        all(vapply(
            period_index_by_subject,
            function(index) !is.unsorted(index),
            logical(1)
        )),
        msg = paste(
            "Positive-duration missing cells must be ordered by period",
            "within each subject"
        )
    )
    if (is.null(prepared$rate_multiplier)) {
        prepared$rate_multiplier <- rep(1, length(prepared$id))
    }
    if (is.null(prepared$fixed_lambda_rate)) {
        prepared$fixed_lambda_rate <- rep(NA_real_, length(prepared$id))
    }

    mu_observed <- numeric(length(prepared$id))
    mu_observed[observed_available] <- prepared$duration[observed_available] *
        exp(
            prepared$design_observed[observed_available, , drop = FALSE] %*%
                sample$beta
        )
    mu_missing <- numeric(length(prepared$id))
    mu_missing[needs_random_draw] <- prepared$duration[needs_random_draw] *
        exp(
            prepared$design_missing[needs_random_draw, , drop = FALSE] %*%
                sample$beta
        )

    observed_count <- ifelse(observed_available, prepared$outcome, 0)
    sum_by_subject <- function(x) {
        totals <- numeric(length(prepared$subject_ids))
        grouped <- rowsum(x, prepared$subject_index, reorder = FALSE)
        totals[as.integer(rownames(grouped))] <- grouped[, 1]
        totals
    }
    observed_count_total <- sum_by_subject(observed_count)
    observed_mu_total <- sum_by_subject(mu_observed)

    result <- numeric(length(prepared$id))
    missing_rows_by_subject <- split(
        which(needs_random_draw),
        factor(
            prepared$subject_index[needs_random_draw],
            levels = seq_along(prepared$subject_ids)
        ),
        drop = TRUE
    )

    for (missing_rows in missing_rows_by_subject) {
        subject <- prepared$subject_index[missing_rows[1]]
        phi_group <- ife(
            prepared$strategy[missing_rows[1]] %in% c("JR", "CR"),
            prepared$reference_group[missing_rows[1]],
            prepared$own_group[missing_rows[1]]
        )
        if (length(sample$phi) == 1) {
            inv_phi <- 1 / sample$phi
        } else {
            assert_that(
                phi_group %in% names(sample$phi),
                msg = paste(
                    "The count-model dispersion draws do not cover all",
                    "required treatment groups"
                )
            )
            inv_phi <- 1 / unname(sample$phi[phi_group])
        }

        size <- inv_phi + observed_count_total[subject]
        conditioning_mass <- inv_phi + observed_mu_total[subject]
        for (missing_row in missing_rows) {
            imputation_mean <- ife(
                is.na(prepared$fixed_lambda_rate[missing_row]),
                mu_missing[missing_row] *
                    prepared$rate_multiplier[missing_row],
                prepared$duration[missing_row] *
                    prepared$fixed_lambda_rate[missing_row] *
                    inv_phi
            )
            prob <- conditioning_mass /
                (conditioning_mass + imputation_mean)
            assert_that(
                is.finite(size),
                size > 0,
                is.finite(prob),
                prob > 0 & prob <= 1,
                msg = "Invalid conditional negative binomial parameters"
            )
            draw <- stats::rnbinom(1, size = size, prob = prob)
            result[missing_row] <- draw
            size <- size + draw
            conditioning_mass <- conditioning_mass + imputation_mean
        }
    }
    result
}
