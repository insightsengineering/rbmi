


#' Last Observation Carried Forward
#'
#' Returns a vector after applied last observation carried forward imputation
#' @param x a vector
#' @export
locf <- function(x) {
    inds <- cumsum(!is.na(x))
    x[inds > 0L] <- x[!is.na(x)][inds]
    x
}





#' Expand dataframes
#'
#' Expands specific variables of a dataframe to ensure all combinations are present
#'
#' @param data Dataset to expand
#' @param ... variables and the levels that should be expanded out
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

    ord <- do.call(base::order, df_expanded[, names(vars)])
    df_return <- df_expanded[ord, names(data)]
    class(df_return) <- class(data)
    rownames(df_return) <- NULL
    return(df_return)
}




#' Fill missing values using locf
#'
#' Imputes missing values in a dataframe using last observation carried forward
#' imputation
#'
#' @param data Dataset to impute missing values in
#' @param vars variables that need to be imputed (character vector)
#' @param group variables to group `vars` by when performing locf (character vector)
#' @param order additional variables to sort by before performing locf (character vector)
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
        group_index <- do.call(as_strata, data_sorted[, group])
    } else {
        group_index <- rep(1, nrow(data))
    }

    assert_that(
        identical(group_index, group_index[base::order(group_index)]),
        msg = "Something has gone wrong..."
    )

    for (var in vars) {
        data_sorted[[var]] <- unlist(
            tapply(data_sorted[[var]], group_index, locf),
            use.names = FALSE
        )
    }

    ## Restore orginal data sorting
    data_return <- data_sorted[base::order(ord), ]
    return(data_return)

}


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




#' Expand and impute with locf
#'
#' @param data TODO
#' @param ... TODO
#' @param fill_vars TODO
#' @param fill_group TODO
#'
#' @export
expand_locf <- function(data, ..., fill_vars, fill_group) {
    data_expanded <- expand(data, ...)
    data_filled <- fill_locf(
        data = data_expanded,
        vars = fill_vars,
        group = fill_group,
        order = names(list(...))
    )
    return(data_filled)
}
