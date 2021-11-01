


#' Last Observation Carried Forward
#'
#' Returns a vector after applied last observation carried forward imputation.
#' @param x a vector.
#'
#' @examples
#' \dontrun{
#' locf(c(NA, 1,2,3,NA,4)) # Returns c(NA, 1, 2, 3, 3, 4)
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
#' @param data Dataset to expand or fill in.
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
        ord <- do.call(base::order, data[, unique(c(group, order))])
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

    for (var in vars) {
        new_vals_list <- tapply(data_sorted[[var]], group_index, locf)
        new_vals <- do.call("c", new_vals_list)
        names(new_vals) <- NULL
        data_sorted[[var]] <- new_vals
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
