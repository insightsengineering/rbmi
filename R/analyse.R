



#' Title
#'
#' @description
#' TODO
#'
#' @param imputations TODO
#' @param fun TODO
#' @param delta TODO
#' @param ... TODO
#'
#' @export
analyse <- function(imputations, fun, delta = NULL, ...) {
    assert_that(
        is.function(fun),
        msg = "`fun` must be a function"
    )

    assert_that(
        is.null(delta) | is.data.frame(delta),
        msg = "`delta` must be NULL or a data.frame"
    )

    vars <- imputations$data$vars

    if( !is.null(delta)){
        expected_vars <- c(
            vars$subjid,
            vars$visit,
            "delta",
        )
        assert_that(
            all(expected_vars %in% names(delta)),
            msg = sprintf(
                "The following variables must exist witin `delta`: `%s`",
                paste0(expected_vars, collapse = "`, `")
            )
        )
    }

    results <- lapply(
        imputations$imputations,
        function(x) {
            dat <- imputations$data$get_data(x)
            dat2 <- apply_delta(
                dat,
                delta,
                group = c(vars$subjid, vars$visit),
                outcome = vars$outcome
            )
            fun(dat2, ...)
        }
    )

    validate_analysis_results(results)

    new_class <- switch(class(imputations$method),
        bayes = "rubin",
        approxbayes = "rubin",
        condmean = ifelse(
            imputations$method$type == "jackknife",
            "jackknife",
            "bootstrap"
        )
    )

    class(results) <- new_class

}



#' Validate analysis results objects
#'
#' @description
#' TODO
#'
#' @param results TODO
validate_analysis_results <- function(results){

    assert_that(
        is.list(results),
        msg = "Analysis results must be a list"
    )

    assert_that(
        all(vapply(results, is.list, logical(1))),
        all(vapply(results, function(x) !is.null(names(x)), logical(1))),
        msg = "Individual analysis results must be a named list"
    )

    results_names <- lapply(results, names)
    results_names_len <- vapply(results_names, length, numeric(1))
    unique_names <- unique(unlist(results_names, use.names = FALSE))

    assert_that(
        length(unique(results_names_len)) == 1,
        length(unique_names) == results_names_len[1],
        msg = "All analysis results must contain the exact same named elements"
    )

    results_unnested <- unlist(results, recursive = FALSE, use.names = FALSE)

    devnull <- lapply(
        results_unnested,
        function(x){
            assert_that(
                is.list(x),
                all(c("est", "se", "df") %in% names(x)),
                msg = "Each analysis result element must be a list with elements `est`, `se` & `df`"
            )
        }
    )
    return(invisible(TRUE))
}


#' Extract imputated datasets
#'
#' @description
#' TODO
#'
#' @param imputations TODO
#' @param index TODO
#' @export
extract_imputed_dfs <- function(
    imputations,
    index = seq_along(imputations$imputations)
){
    x <- imputations$imputations[index]
    lapply(
        x,
        function(x) imputations$data$get_data(x)
    )
}


