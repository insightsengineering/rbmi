


#' Last Observation Carried Forward
#'
#' Returns a vector after applied last observation carried forward imputation.
#' @param x a vector.
#'
#' @examples
#' \dontrun{
#' locf(c(NA, 1, 2, 3, NA, 4)) # Returns c(NA, 1, 2, 3, 3, 4)
#' }
#' @export
locf <- function(x) {
    inds <- cumsum(!is.na(x))
    x[inds > 0L] <- x[!is.na(x)][inds]
    x
}





#' Expand and fill in missing `data.frame` rows
#'
#' These functions are essentially wrappers around [base::expand.grid()] to ensure that missing
#' combinations of data are inserted into a `data.frame` with imputation/fill methods for updating
#' covariate values of newly created rows.
#'
#' @param data dataset to expand or fill in.
#' @param ... variables and the levels that should be expanded out (note that duplicate entries of
#' levels will result in multiple rows for that level).
#' @param vars character vector containing the names of variables that need to be filled in.
#' @param group character vector containing the names of variables to group
#' by when performing LOCF imputation of `var`.
#' @param order character vector containing the names of additional variables to sort the `data.frame`
#' by before performing LOCF.
#'
#' @details
#'
#' The [draws()] function makes the assumption that all subjects and visits are present
#' in the `data.frame` and that all covariate values are non missing; `expand()`,
#' `fill_locf()` and `expand_locf()` are utility functions to support users in ensuring
#' that their `data.frame`'s conform to these assumptions.
#'
#' `expand()` takes vectors for expected levels in a `data.frame` and expands out all
#' combinations inserting any missing rows into the `data.frame`. Note that all "expanded"
#' variables are cast as factors.
#'
#' `fill_locf()` applies LOCF imputation to named covariates to fill in any NAs created
#' by the insertion of new rows by `expand()` (though do note that no distinction is
#' made between existing NAs and newly created NAs). Note that the `data.frame` is sorted
#' by `c(group, order)` before performing the LOCF imputation; the `data.frame`
#' will be returned in the original sort order however.
#'
#' `expand_locf()` a simple composition function of `fill_locf()` and `expand()` i.e.
#' `fill_locf(expand(...))`.
#'
#' ## Missing First Values
#'
#' The `fill_locf()` function performs last observation carried forward imputation.
#' A natural consquence of this is that it is unable to impute missing observations if the
#' observation is the first value.
#' A common request is for this function to impute the first value if it is missing
#' however doing so risks silent errors in the case of time varying covariates.
#' The current recommendation for dealing with this problem is to first use `expand_locf()` on just
#' the visits and time varying covariates and then merge on the baseline covariates afterwards i.e.
#'
#' ```
#' library(dplyr)
#' 
#' dat_expanded <- expand(
#'     data = dat,
#'     subject = c("pt1", "pt2", "pt3", "pt4"),
#'     visit = c("vis1", "vis2", "vis3")
#' )
#'
#' dat_filled %>%
#'     left_join(baseline_covariates, by = "subject")
#' ```
#'
#' @examples
#' \dontrun{
#' dat_expanded <- expand(
#'     data = dat,
#'     subject = c("pt1", "pt2", "pt3", "pt4"),
#'     visit = c("vis1", "vis2", "vis3")
#' )
#'
#' dat_filled <- fill_loc(
#'     data = dat_expanded,
#'     vars = c("Sex", "Age"),
#'     group = "subject",
#'     order = "visit"
#' )
#'
#' ## Or
#'
#' dat_filled <- expand_locf(
#'     data = dat,
#'     subject = c("pt1", "pt2", "pt3", "pt4"),
#'     visit = c("vis1", "vis2", "vis3"),
#'     vars = c("Sex", "Age"),
#'     group = "subject",
#'     order = "visit"
#' )
#' }
#' @export
expand <- function(data, ...) {
    vars <- list(...)

    assert_variables_exist(data, names(vars))

    for (var in names(vars)) {
        df_val <- unique(data[[var]])

        assert_that(
            is.character(df_val) | is.factor(df_val),
            msg = sprintf(
                "Variable `%s` is neither character nor factor. Cannot expand non-categorical variables",
                var
            )
        )

        assert_that(
            all(df_val %in% vars[[var]]),
            msg = sprintf(paste(
                "Variable `%s` contains values/levels that were not specified.",
                "Please remove any levels that are not required prior to using this function"
            ), var)
        )
    }
    reference <- expand.grid(vars, stringsAsFactors = FALSE)
    df_expanded <- merge(reference, data, by = names(vars), all.x = TRUE)

    for (var in names(vars)) {
        df_expanded[[var]] <- factor(df_expanded[[var]], levels = vars[[var]])
    }

    df_return <- sort_by(df_expanded, names(vars))[, names(data), drop = FALSE]
    class(df_return) <- class(data)
    rownames(df_return) <- NULL
    return(df_return)
}




#' @rdname expand
#' @export
fill_locf <- function(data, vars, group = NULL, order = NULL) {

    if (!is.null(group)) {
        assert_that(
            is.character(group),
            length(group) > 0,
            msg = "`group` must be NULL or a character vector"
        )
    }

    if (!is.null(order)) {
        assert_that(
            is.character(order),
            length(order) > 0,
            msg = "`order` must be NULL or a character vector"
        )
    }

    assert_variables_exist(data, vars)

    if (is.null(c(group, order))) {
        ord <- seq_len(nrow(data))
    } else {
        ord <- do.call(base::order, data[, unique(c(group, order)), drop = FALSE])
    }

    data_sorted <- data[ord, ]

    if (!is.null(group)) {
        group_index <- do.call(as_strata, data_sorted[, group, drop = FALSE])
    } else {
        group_index <- rep(1, nrow(data))
    }

    assert_that(
        identical(group_index, group_index[base::order(group_index)]),
        msg = "Something has gone wrong..."
    )

    warn_vars <- character(0)
    for (var in vars) {
        vals <- data_sorted[[var]]
        vals_first <- tapply(vals, group_index, function(x) x[1])
        is_missing <- vapply(vals_first, function(x) any(is.na(x)), logical(1))
        if (any(is_missing)) {
            warn_vars <- c(warn_vars, sprintf("`%s`", var))
        }
        new_vals_list <- tapply(vals, group_index, locf)
        new_vals <- unlist(new_vals_list, recursive = FALSE, use.names = FALSE)
        attributes(new_vals) <- attributes(vals)
        data_sorted[[var]] <- new_vals
    }


    if (length(warn_vars) > 0) {
        warn <- sprintf(
                paste(
                    "The following variables have missing values as their first value in one of more",
                    "groups: %s\n",
                    "Please consult the man page for `fill_locf()` for further",
                    "details / recommendations"
                ),
                paste(warn_vars, collapse = ", ")
            )
            warning(warn)
    }

    ## Restore orginal data sorting
    data_return <- data_sorted[base::order(ord), ]
    return(data_return)

}


#' Assert that all variables exist within a dataset
#'
#' Performs an assertion check to ensure that a vector of variable exists within a data.frame as expected.
#'
#' @param data a data.frame
#' @param vars a character vector of variable names
assert_variables_exist <- function(data, vars) {

    assert_that(
        is.data.frame(data)
    )

    assert_that(
        is.character(vars),
        length(vars) >= 0,
        msg = "`vars` must be a character vector"
    )

    for (var in vars) {
        assert_that(
            var %in% names(data),
            msg = sprintf("Variable `%s` does not exist in `data`", var)
        )
    }
    return(TRUE)
}




#' @rdname expand
#' @export
expand_locf <- function(data, ..., vars, group, order) {
    data_expanded <- expand(data, ...)
    data_filled <- fill_locf(
        data = data_expanded,
        vars = vars,
        group = group,
        order = order
    )
    return(data_filled)
}
