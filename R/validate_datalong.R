#' Validate a `longdata` object
#'
#' @name validate_datalong
#'
#' @param data a `data.frame` containing the longitudinal outcome data + covariates
#' for multiple subjects
#'
#' @param vars a `vars` object as created by [set_vars()]
#'
#' @param data_ice a `data.frame` containing the subjects ICE data. See [draws()] for details.
#'
#' @param update logical, indicates if the ICE data is being set for the first time or if an update
#' is being applied
#'
#' @details
#' These functions are used to validate various different parts of the `longdata` object
#' to be used in [draws()], [impute()], [analyse()] and [pool()]. In particular:
#'
#' - `validate_datalong_varExists` - Checks that each variable listed in `vars` actually exists
#' in the `data`
#'
#' - `validate_datalong_types` - Checks that the types of each key variable is as expected
#' i.e. that visit is a factor variable, or period is a character, factor, or
#' numeric variable when `period` is specified via [set_vars()]
#'
#' - `validate_datalong_notMissing` - Checks that none of the key variables (except the outcome variable)
#' contain any missing values
#'
#' - `validate_datalong_complete` - Checks that `data` is complete i.e. there is 1 row for each subject *
#' visit combination. e.g. that `nrow(data) == length(unique(subjects)) * length(unique(visits))`
#'
#' - `validate_datalong_uniformStrata` - Checks to make sure that any variables listed as stratification
#' variables do not vary over time. e.g. that subjects don't switch between stratification groups.
#'
validate_datalong <- function(data, vars) {
    validate_datalong_varExists(data, vars)
    validate_datalong_types(data, vars)
    validate_datalong_notMissing(data, vars)
    validate_datalong_complete(data, vars)
    validate_datalong_uniformStrata(data, vars)
    return(invisible(TRUE))
}


#' @rdname validate_datalong
validate_datalong_varExists <- function(data, vars) {
    assert_that(
        vars$outcome %in% names(data),
        msg = sprintf("Cannot find %s in `data`", vars$outcome)
    )

    assert_that(
        vars$group %in% names(data),
        msg = sprintf("Cannot find %s in `data`", vars$group)
    )

    if (uses_visit(vars)) {
        assert_that(
            vars$visit %in% names(data),
            msg = sprintf("Cannot find %s in `data`", vars$visit)
        )
    }

    assert_that(
        vars$subjid %in% names(data),
        msg = sprintf("Cannot find %s in `data`", vars$subjid)
    )

    if (uses_period(vars)) {
        assert_that(
            vars$period %in% names(data),
            msg = sprintf("Cannot find %s in `data`", vars$period)
        )

        assert_that(
            vars$duration %in% names(data),
            msg = sprintf("Cannot find %s in `data`", vars$duration)
        )
    }

    assert_that(
        all(vars$strata %in% names(data)) | is.null(vars$strata),
        msg = "One of more variables listed in `vars$strata` do not exist in `data`"
    )

    covars <- extract_covariates(vars$covariates)
    assert_that(
        all(covars %in% names(data)) | is.null(covars),
        msg = "One of more variables listed in `vars$covariates` do not exist in `data`"
    )

    return(invisible(TRUE))
}

#' @rdname validate_datalong
validate_datalong_types <- function(data, vars) {
    covars <- extract_covariates(vars$covariates)

    assert_that(
        is.factor(data[[vars$subjid]]),
        msg = sprintf("Variable `%s` should be of type factor", vars$subjid)
    )

    assert_that(
        is.factor(data[[vars$group]]),
        msg = sprintf("Variable `%s` should be of type factor", vars$group)
    )

    assert_that(
        length(unique(data[[vars$group]])) == nlevels(data[[vars$group]]),
        msg = sprintf(
            "The number of levels in variable `%s` is different than the number of observed levels",
            vars$group
        )
    )

    if (uses_period(vars)) {
        period_values <- data[[vars$period]]
        period_values <- period_values[!is.na(period_values)]
        valid_type <- is.character(data[[vars$period]]) ||
            is.factor(data[[vars$period]]) ||
            is.numeric(data[[vars$period]])
        finite_numeric <- !is.numeric(data[[vars$period]]) ||
            all(is.finite(period_values))
        assert_that(
            valid_type,
            finite_numeric,
            msg = sprintf(
                "Variable `%s` should be of type character, factor or numeric",
                vars$period
            )
        )
    } else {
        assert_that(
            is.factor(data[[vars$visit]]),
            msg = sprintf("Variable `%s` should be of type factor", vars$visit)
        )

        assert_that(
            length(unique(data[[vars$visit]])) == nlevels(data[[vars$visit]]),
            msg = sprintf(
                "The number of levels in variable `%s` is different than the number of observed levels",
                vars$visit
            )
        )
    }

    assert_that(
        is.numeric(data[[vars$outcome]]),
        msg = sprintf("Variable `%s` should be of type numeric", vars$outcome)
    )

    if (uses_period(vars)) {
        duration <- data[[vars$duration]]
        duration <- duration[!is.na(duration)]
        assert_that(
            is.numeric(data[[vars$duration]]),
            all(is.finite(duration)),
            all(duration >= 0),
            msg = sprintf(
                "Variable `%s` should be numeric, finite and greater than or equal to zero, apart from missing values",
                vars$duration
            )
        )
    }

    additional_vars <- c(covars, vars$strata)

    if (!is.null(additional_vars)) {
        for (var in additional_vars) {
            assert_that(
                is_num_char_fact(data[[var]]),
                msg = sprintf(
                    "Variable `%s` should be of type numeric, factor or character",
                    var
                )
            )
        }
        for (var in covars) {
            if (is_char_fact(data[[var]])) {
                assert_that(
                    length(unique(data[[var]])) >= 2,
                    msg = sprintf(
                        "Covariate `%s` only has 1 level, categorical covariates must have at least 2 levels",
                        var
                    )
                )
            }
        }
    }
    return(invisible(TRUE))
}

#' @rdname validate_datalong
validate_datalong_notMissing <- function(data, vars) {
    non_missing_variables <- c(
        vars$group,
        vars$visit,
        vars$period,
        vars$subjid,
        vars$strata,
        extract_covariates(vars$covariates)
    )
    if (uses_period(vars)) {
        non_missing_variables <- c(non_missing_variables, vars$duration)
    }
    for (var in non_missing_variables) {
        if (any(is.na(data[[var]]))) {
            stop(paste0("Variable ", var, " contains missing data"))
        }
    }
    return(invisible(TRUE))
}

#' @rdname validate_datalong
validate_datalong_complete <- function(data, vars) {
    unique_subjects <- unique(data[[vars$subjid]])

    if (uses_visit(vars)) {
        unique_visits <- visit_levels(data, vars)

        data_dedup <- unique(data[, c(vars$subjid, vars$visit)])

        assert_that(
            nrow(data) == length(unique_subjects) * length(unique_visits),
            nrow(data_dedup) == nrow(data),
            msg = "At least one subject has either incomplete or duplicated data"
        )
    } else {
        assert_that(uses_period(vars))

        data_dedup <- unique(data[, c(vars$subjid, vars$period)])

        assert_that(
            nrow(data_dedup) == nrow(data),
            msg = "At least one subject has duplicated period data"
        )
    }
    return(invisible(TRUE))
}

#' @rdname validate_datalong
validate_datalong_uniformStrata <- function(data, vars) {
    for (var in vars$strata) {
        x <- tapply(
            data[[var]],
            data[[vars$subjid]],
            function(x) length(unique(x))
        )
        if (!all(x == 1)) {
            stop(
                "Stratification variable '",
                var,
                "' is not constant within at least one subject"
            )
        }
    }
    return(invisible(TRUE))
}


#' @rdname validate_datalong
validate_dataice <- function(data, data_ice, vars, update = FALSE) {
    validate(vars)

    strategy <- vars$strategy
    subjid <- vars$subjid

    assert_that(
        is.character(data_ice[[strategy]]),
        all(!is.na(data_ice[[strategy]])),
        msg = "`data_ice[[vars$strategy]]` must be a non-missing character vector"
    )

    assert_that(
        is_char_fact(data_ice[[subjid]]),
        all(!is.na(data_ice[[subjid]])),
        msg = "`data_ice[[vars$subjid]]` must be a non-missing character or factor vector"
    )

    assert_that(
        all(as.character(data_ice[[subjid]]) %in% as.character(data[[subjid]])),
        msg = "`data_ice[[vars$subjid]]` contains values that aren't in `data[[vars$subjid]]`"
    )

    if (!update) {
        if (uses_visit(vars)) {
            visit <- vars$visit
            valid_visits <- unique(as.character(data[[visit]]))

            assert_that(
                is.character(data_ice[[visit]]) | is.factor(data_ice[[visit]]),
                all(!is.na(data_ice[[visit]])),
                msg = "`data_ice[[vars$visit]]` must be a non-missing character or factor vector"
            )

            assert_that(
                all(as.character(data_ice[[visit]]) %in% valid_visits),
                msg = "`data_ice[[vars$visit]]` contains values that are not in `data[[vars$visit]]`"
            )
        }

        # Note: When period is used, then data_ice does not need
        # to contain a period variable.
    }

    assert_that(
        length(data_ice[[subjid]]) == length(unique(data_ice[[subjid]])),
        msg = paste(
            "`data_ice` must contain at most 1 row per subjects.",
            "If you have multiple ICEs please use the first Non-MAR ICE"
        )
    )

    return(TRUE)
}

#' Check whether a variable specification uses visits
#'
#' @param vars An `ivars` object created by [set_vars()].
#'
#' @return A single logical value.
#'
#' @keywords internal
uses_visit <- function(vars) {
    !is.null(vars$visit)
}

#' Check whether a variable specification uses periods
#'
#' @param vars An `ivars` object created by [set_vars()].
#'
#' @return A single logical value.
#'
#' @keywords internal
uses_period <- function(vars) {
    !is.null(vars$period)
}

#' Get ordered period levels from longitudinal data
#'
#' @param data A longitudinal data frame.
#' @param vars An `ivars` object created by [set_vars()].
#'
#' @return A character vector containing observed period levels. Factor level
#'   order is retained, numeric periods are sorted numerically, and otherwise
#'   order of first appearance is used.
#'
#' @keywords internal
period_levels <- function(data, vars) {
    period <- data[[vars$period]]
    observed <- unique(as.character(period))
    if (is.factor(period)) {
        levels(period)[levels(period) %in% observed]
    } else if (is.numeric(period)) {
        as.character(sort(unique(period)))
    } else {
        observed
    }
}

#' Get visit or period levels from longitudinal data
#'
#' @param data A longitudinal data frame.
#' @param vars An `ivars` object created by [set_vars()].
#'
#' @return A character vector containing the applicable visit or period levels.
#'
#' @keywords internal
visit_levels <- function(data, vars) {
    if (uses_period(vars)) {
        period_levels(data, vars)
    } else {
        levels(data[[vars$visit]])
    }
}
