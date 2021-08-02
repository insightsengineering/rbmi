



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
            "delta"
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
    validate_analyse(results)
    return(results)
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


