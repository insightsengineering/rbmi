#' Simulate longitudinal count data
#'
#' Simulates data from the negative multinomial model used for count outcomes in
#' `rbmi`. Each subject has a gamma-distributed frailty shared by all periods,
#' so the marginal period counts are negative binomial and counts from the same
#' subject are correlated.
#'
#' @param n Number of subjects per treatment group. Either one positive integer
#'   or one positive integer for each element of `groups`.
#' @param groups Character vector containing the treatment-group levels.
#' @param periods Character, numeric, or factor vector containing the ordered
#'   period values.
#' @param log_rates Numeric matrix of marginal log event rates, with one row per
#'   treatment group and one column per period. If `NULL`, all log rates are 0.
#' @param duration Positive numeric exposure for each period. Either a scalar or
#'   a vector with one value per period.
#' @param phi Positive frailty variance, or dispersion parameter.
#'   Either a scalar shared by all groups or
#'   one value per treatment group. Conditional on its frailty, a subject's
#'   period counts are independent Poisson variables. The frailty has mean 1
#'   and variance `phi`.
#' @param baseline_effect Numeric coefficient for a standard-normal,
#'   subject-level baseline covariate on the log-rate scale.
#' @param prob_miss Probability that the outcome is missing in each period.
#'   Either a scalar or one value per period. Missingness is generated
#'   independently after the complete counts have been simulated.
#'
#' @return A `data.frame` with one row per subject and period and columns `id`,
#'   `period`, `duration`, `group`, `baseline`, `outcome_full`, and `outcome`.
#'   The `outcome_full` column contains the count before missingness is applied.
#'
#' @examples
#' set.seed(123)
#' log_rates <- rbind(
#'     Control = log(c(1.0, 0.8, 0.8)),
#'     Intervention = log(c(1.4, 1.2, 1.2))
#' )
#' data <- simulate_count_data(
#'     n = 20,
#'     groups = rownames(log_rates),
#'     periods = as.character(1:3),
#'     log_rates = log_rates,
#'     duration = c(1, 1, 1),
#'     phi = 0.4,
#'     prob_miss = c(0, 0, 0.3)
#' )
#' head(data)
#'
#' @export
simulate_count_data <- function(
    n = 100,
    groups = c("Control", "Intervention"),
    periods = as.character(1:3),
    log_rates = NULL,
    duration = 1,
    phi = 0.5,
    baseline_effect = 0,
    prob_miss = 0
) {
    assert_that(
        is.character(groups),
        length(groups) >= 1,
        !anyNA(groups),
        all(nzchar(groups)),
        !anyDuplicated(groups),
        msg = "`groups` must contain unique, non-missing character values"
    )
    assert_that(
        (is.character(periods) || is.numeric(periods) || is.factor(periods)),
        length(periods) >= 1,
        !anyNA(periods),
        all(nzchar(as.character(periods))),
        !anyDuplicated(as.character(periods)),
        msg = "`periods` must contain unique, non-missing values"
    )
    periods <- as.character(periods)

    assert_that(
        is.numeric(n),
        length(n) %in% c(1, length(groups)),
        all(is.finite(n)),
        all(n > 0),
        all(n == trunc(n)),
        msg = "`n` must contain positive integers, one per treatment group"
    )
    n <- rep_len(n, length(groups))
    n <- as.integer(n)

    if (is.null(log_rates)) {
        log_rates <- matrix(0, nrow = length(groups), ncol = length(periods))
    }
    assert_that(
        is.numeric(log_rates),
        is.matrix(log_rates),
        identical(dim(log_rates), c(length(groups), length(periods))),
        all(is.finite(log_rates)),
        msg = paste(
            "`log_rates` must be a finite numeric matrix with one row per",
            "group and one column per period"
        )
    )

    assert_that(
        is.numeric(duration),
        length(duration) %in% c(1, length(periods)),
        all(is.finite(duration)),
        all(duration > 0),
        msg = "`duration` must contain positive values, one per period"
    )
    assert_that(
        is.numeric(phi),
        length(phi) %in% c(1, length(groups)),
        all(is.finite(phi)),
        all(phi > 0),
        msg = "`phi` must contain positive values, one per treatment group"
    )
    assert_that(
        is.numeric(baseline_effect),
        length(baseline_effect) == 1,
        is.finite(baseline_effect),
        msg = "`baseline_effect` must be one finite number"
    )
    assert_that(
        is.numeric(prob_miss),
        length(prob_miss) %in% c(1, length(periods)),
        all(is.finite(prob_miss)),
        all(prob_miss >= 0 & prob_miss <= 1),
        msg = "`prob_miss` must contain probabilities, one per period"
    )
    duration <- rep_len(duration, length(periods))
    phi <- rep_len(phi, length(groups))
    prob_miss <- rep_len(prob_miss, length(periods))

    number_subjects <- sum(n)
    number_periods <- length(periods)
    group_index_subject <- rep(seq_along(groups), n)
    group_index <- rep(group_index_subject, each = number_periods)
    subject_index <- seq_len(number_subjects)
    baseline_subject <- stats::rnorm(number_subjects)
    frailty_subject <- stats::rgamma(
        number_subjects,
        shape = 1 / phi[group_index_subject],
        rate = 1 / phi[group_index_subject]
    )
    period_index <- rep(seq_len(number_periods), number_subjects)
    linear_predictor <- log_rates[cbind(group_index, period_index)] +
        baseline_effect * rep(baseline_subject, each = number_periods)
    mean_count <- rep(duration, number_subjects) *
        exp(linear_predictor) *
        rep(frailty_subject, each = number_periods)
    outcome_full <- stats::rpois(length(mean_count), mean_count)
    is_missing <- as.logical(stats::rbinom(
        length(mean_count),
        size = 1,
        prob = rep(prob_miss, number_subjects)
    ))

    outcome <- outcome_full
    outcome[is_missing] <- NA_integer_

    data.frame(
        id = factor(
            rep(paste0("id_", subject_index), each = number_periods),
            levels = paste0("id_", subject_index)
        ),
        period = factor(
            rep(periods, number_subjects),
            levels = periods,
            ordered = TRUE
        ),
        duration = rep(duration, number_subjects),
        group = factor(groups[group_index], levels = groups),
        baseline = rep(baseline_subject, each = number_periods),
        outcome_full = outcome_full,
        outcome = outcome
    )
}
