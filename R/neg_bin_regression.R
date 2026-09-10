#' Negative Binomial Regression for Count Outcomes
#'
#' Fits a negative binomial regression model to a completed count-outcome
#' dataset. The period-specific counts and durations are first summed within
#' each subject. The model is then fitted to the total count with the logarithm
#' of total duration as an offset.
#'
#' All regression coefficients are returned on the model's log-rate scale so
#' that they can be pooled with Rubin's rules by [pool()]. For reporting,
#' `neg_bin_regression()` has an exponential transformation attached, so
#' [analyse()] and [pool()] report its pooled parameters on the rate-ratio
#' scale by default. This includes the
#' intercept, the treatment-group coefficients, and coefficients for any other
#' terms in `vars$covariates`. The first level of `vars$group` is used as the
#' reference group.
#'
#' @param data A completed count-outcome `data.frame`, normally supplied by
#'   [analyse()]. It must contain one row per subject and period.
#' @param vars A `vars` object created by [set_vars()]. The `subjid`, `outcome`,
#'   `group`, `period`, and `duration` elements are required. Any variables in
#'   `covariates` must be constant within subject.
#'
#' @return A named list with one element for every fitted regression
#'   coefficient. Each element contains the coefficient estimate (`est`) on
#'   the log-rate scale, its standard error (`se`), and infinite degrees of
#'   freedom (`df`) for asymptotic normal inference. The offset and negative
#'   binomial dispersion parameter are not included.
#'
#' @details
#' The model is
#' `total count ~ treatment group + covariates + offset(log(total duration))`.
#' It is fitted by maximum likelihood using [MASS::glm.nb()]. The covariance
#' matrix jointly accounts for estimation of the regression coefficients and
#' the negative binomial dispersion parameter, matching the calculation used
#' by SAS `PROC GENMOD`.
#'
#' The outcome column keeps the name supplied in `vars$outcome`; imputation
#' replaces its missing values in place. `neg_bin_regression()` performs the
#' subject-level aggregation internally, so a separate `Imputed_Count` column
#' is neither created nor required.
#'
#' @seealso [analyse()], [pool()], [set_vars()], [MASS::glm.nb()]
#' @export
neg_bin_regression <- function(data, vars) {
    validate(vars)

    subjid <- vars[["subjid"]]
    outcome <- vars[["outcome"]]
    group <- vars[["group"]]
    duration <- vars[["duration"]]
    covariates <- vars[["covariates"]]
    covariate_vars <- extract_covariates(covariates)
    required_vars <- unique(c(
        subjid,
        outcome,
        group,
        vars[["period"]],
        duration,
        covariate_vars
    ))

    assert_that(
        all(required_vars %in% names(data)),
        msg = sprintf(
            "The following variables do not exist in `data`: `%s`",
            paste0(setdiff(required_vars, names(data)), collapse = "`, `")
        )
    )
    assert_that(
        is.factor(data[[group]]),
        nlevels(data[[group]]) >= 2,
        length(unique(data[[group]])) == nlevels(data[[group]]),
        msg = "`data[[vars$group]]` must be a factor with at least 2 observed levels"
    )
    assert_that(
        is.numeric(data[[outcome]]),
        !anyNA(data[[outcome]]),
        all(is.finite(data[[outcome]])),
        all(data[[outcome]] >= 0),
        all(data[[outcome]] == trunc(data[[outcome]])),
        msg = "`data[[vars$outcome]]` must contain finite, non-negative integer counts"
    )
    assert_that(
        is.numeric(data[[duration]]),
        !anyNA(data[[duration]]),
        all(is.finite(data[[duration]])),
        all(data[[duration]] >= 0),
        msg = "`data[[vars$duration]]` must contain finite, non-negative values"
    )

    subject_vars <- unique(c(group, covariate_vars))
    rows_by_subject <- split(seq_len(nrow(data)), data[[subjid]], drop = TRUE)
    is_constant <- vapply(
        rows_by_subject,
        function(rows) {
            all(vapply(
                subject_vars,
                function(var) length(unique(data[[var]][rows])) == 1,
                logical(1)
            ))
        },
        logical(1)
    )
    assert_that(
        all(is_constant),
        msg = paste(
            "`vars$group` and all analysis covariates must be constant within subject;",
            sprintf(
                "this is not true for subject(s): `%s`",
                paste0(names(rows_by_subject)[!is_constant], collapse = "`, `")
            )
        )
    )

    first_rows <- vapply(rows_by_subject, `[[`, integer(1), 1)
    subject_data <- data[
        first_rows,
        unique(c(subjid, subject_vars)),
        drop = FALSE
    ]
    subject_data[[outcome]] <- vapply(
        rows_by_subject,
        function(rows) sum(data[[outcome]][rows]),
        numeric(1)
    )
    subject_data[[duration]] <- vapply(
        rows_by_subject,
        function(rows) sum(data[[duration]][rows]),
        numeric(1)
    )
    assert_that(
        all(subject_data[[duration]] > 0),
        msg = "Total `vars$duration` must be greater than zero for every subject"
    )

    # Define the treatment contrasts explicitly so that the returned estimates
    # always compare each non-reference level with the first factor level,
    # independently of the user's global contrasts option.
    stats::contrasts(subject_data[[group]]) <- stats::contr.treatment(
        levels(subject_data[[group]]),
        base = 1
    )

    log_duration <- "..rbmi..count..log_duration"
    assert_that(
        !log_duration %in% names(subject_data),
        msg = sprintf(
            "Variable name `%s` is reserved for internal use",
            log_duration
        )
    )
    subject_data[[log_duration]] <- log(subject_data[[duration]])
    frm <- as_simple_formula(
        outcome,
        c(group, covariates, sprintf("offset(%s)", log_duration))
    )
    model <- MASS::glm.nb(
        formula = frm,
        data = subject_data,
        link = log,
        x = TRUE,
        y = TRUE,
        model = TRUE
    )
    covariance <- glm_nb_covariance(model)

    coefficient_names <- names(stats::coef(model))
    result <- lapply(
        coefficient_names,
        function(coefficient) {
            list(
                est = unname(stats::coef(model)[[coefficient]]),
                se = unname(sqrt(covariance[coefficient, coefficient])),
                df = Inf
            )
        }
    )
    names(result) <- coefficient_names
    result
}

attr(neg_bin_regression, "transform") <- list(
    transform = exp,
    derivative = exp
)

#' Covariance matrix for a negative binomial regression model
#'
#' Computes the inverse observed information for the regression coefficients
#' and the negative binomial shape parameter. Unlike the usual covariance
#' returned by `glm.nb`, this calculation does not assume that the regression
#' coefficients and the estimated dispersion are independent.
#'
#' @param model A model fitted by [MASS::glm.nb()] with `x = TRUE`.
#'
#' @return A covariance matrix containing the regression coefficients and an
#'   additional `dispersion` row and column.
#'
#' @keywords internal
glm_nb_covariance <- function(model) {
    assert_that(
        inherits(model, "negbin"),
        !is.null(model$x),
        msg = "`model` must be a `glm.nb` fit created with `x = TRUE`"
    )

    number_coefficients <- length(stats::coef(model))
    observed_information <- matrix(
        0,
        nrow = number_coefficients + 1,
        ncol = number_coefficients + 1
    )
    mu <- model$fitted.values
    theta <- model$theta
    y <- model$y
    design <- model$x

    beta_weights <- (1 + y / theta) * mu / (1 + mu / theta)^2
    observed_information[
        seq_len(number_coefficients),
        seq_len(number_coefficients)
    ] <- crossprod(design, design * beta_weights)

    dispersion_index <- number_coefficients + 1
    observed_information[dispersion_index, dispersion_index] <- -sum(
        trigamma(theta + y) -
            trigamma(theta) -
            2 / (mu + theta) +
            (theta + y) / (theta + mu)^2 +
            1 / theta
    )

    beta_dispersion <- -colSums(
        design * ((y - mu) * mu / (theta + mu)^2)
    )
    observed_information[dispersion_index, seq_len(number_coefficients)] <-
        beta_dispersion
    observed_information[seq_len(number_coefficients), dispersion_index] <-
        beta_dispersion

    parameter_names <- c(names(stats::coef(model)), "dispersion")
    dimnames(observed_information) <- list(parameter_names, parameter_names)
    solve(observed_information)
}
