

#' Validate a longdata object
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
#' These functions are used to validate various different parts of the longdata object
#' to be used in [draws()], [impute()], [analyse()] and [pool()]. In particular:
#'
#' - validate_datalong_varExists - Checks that each variable listed in `vars` actually exists
#' in the `data`
#'
#' - validate_datalong_types - Checks that the types of each key variable is as expected
#' i.e. that visit is a factor variable
#'
#' - validate_datalong_notMissing - Checks that none of the key variables (except the outcome variable)
#' contain any missing values
#'
#' - validate_datalong_complete - Checks that `data` is complete i.e. there is 1 row for each subject *
#' visit combination. e.g. that `nrow(data) == length(unique(subjects)) * length(unique(visits))`
#'
#' - validate_datalong_unifromStrata - Checks to make sure that any variables listed as stratification
#' variables do not vary over time. e.g. that subjects don't switch between stratification groups.
#'
validate_datalong <- function(data, vars) {
    validate_datalong_varExists(data, vars)
    validate_datalong_types(data, vars)
    validate_datalong_notMissing(data, vars)
    validate_datalong_complete(data, vars)
    validate_datalong_unifromStrata(data, vars)
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

    assert_that(
        vars$visit %in% names(data),
        msg = sprintf("Cannot find %s in `data`", vars$visit)
    )

    assert_that(
        vars$subjid %in% names(data),
        msg = sprintf("Cannot find %s in `data`", vars$subjid)
    )

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
        is.factor(data[[vars$visit]]),
        msg = sprintf("Variable `%s` should be of type factor", vars$visit)
    )

    assert_that(
        is.numeric(data[[vars$outcome]]),
        msg = sprintf("Variable `%s` should be of type numeric", vars$outcome)
    )

    additional_vars <- c(covars, vars$strata)

    if (!is.null(additional_vars)) {
        for (var in additional_vars) {
            assert_that(
                is_num_char_fact(data[[var]]),
                msg = sprintf("Variable `%s` should be of type numeric, factor or character", var)
            )
        }
    }
    return(invisible(TRUE))
}

#' @rdname validate_datalong
validate_datalong_notMissing <- function(data, vars) {
    non_missing_variables <- c(
        vars$group,
        vars$visit,
        vars$subjid,
        vars$strata,
        extract_covariates(vars$covariates)
    )
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
    unique_visits <- levels(data[[vars$visit]])
    is_complete_list <- tapply(
        data[[vars$visit]],
        data[[vars$subjid]],
        function(x) {
            all(unique_visits %in% x) & length(unique_visits) == length(x)
        },
        simplify = FALSE
    )
    is_complete <- unlist(is_complete_list, use.names = FALSE)
    if (! all(is_complete)) {
        stop("At least one subject has either incomplete or duplicate data")
    }
    return(invisible(TRUE))
}

#' @rdname validate_datalong
validate_datalong_unifromStrata <- function(data, vars) {
    for (var in vars$strata) {
        x <- tapply(
            data[[var]],
            data[[vars$subjid]],
            function(x) length(unique(x))
        )
        if (!all(x == 1)) {
            stop(
                "Stratification variable '", var,
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
    visit <- vars$visit
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

    assert_that(
        length(data_ice[[subjid]]) == length(unique(data_ice[[subjid]])),
        msg = paste(
            "`data_ice` must contain at most 1 row per subjects.",
            "If you have multiple ICEs please use the first Non-MAR ICE"
        )
    )

    return(TRUE)
}


