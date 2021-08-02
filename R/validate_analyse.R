#' Validate Analysis Objects
#' 
#' Validates the return object of the analyse() function
#' 
#' @param results A Analysis results object (of class "jackknife", "bootstrap", "rubin")
#' @param pars Expected parameters within each result object
#' @export
validate_analyse <- function(results) {

    assert_that(
        class(results) %in% c("jackknife", "bootstrap", "rubin"),
        msg = "`results` must be of class 'jackknife', 'bootstrap' or 'rubin'"
    )

    UseMethod("validate_analyse")
}


#' @rdname validate_analyse
#' @export
validate_analyse.jackknife <- function(results) {
    validate_analyse_pars(results, "est")
}

#' @rdname validate_analyse
#' @export
validate_analyse.bootstrap <- function(results) {
    validate_analyse_pars(results, "est")
}

#' @rdname validate_analyse
#' @export
validate_analyse.rubin <- function(results){
    validate_analyse_pars(results, c("est", "se", "df"))
}



#' @rdname validate_analyse
validate_analyse_pars <- function(results, pars){

    assert_that(
        length(results) != 0,
        is.list(results),
        all(vapply(results, is.list, logical(1))),
        msg = "Analysis results must be a list of lists"
    )

    assert_that(
        length(names(results[[1]])) != 0,
        all(vapply(results, function(x) !is.null(names(x)) & all(names(x) != ""), logical(1))),
        msg = "Individual analysis results must be named lists"
    )

    results_names <- lapply(results, function(x) unique(names(x)))
    results_names_flat <- unlist(results_names, use.names = FALSE)
    results_names_count <- table(results_names_flat)

    assert_that(
        all(results_names_count == length(results)),
        msg = "Each individual analysis result must contain identically named elements"
    )

    results_unnested <- unlist(results, recursive = FALSE, use.names = FALSE)

    devnull <- lapply(
        results_unnested,
        function(x){
            assert_that(
                is.list(x),
                all(pars %in% names(x)),
                msg = sprintf(
                    "Each individual analysis result element must be a list with elements `%s`",
                    paste0(pars, collapse = "`, `")
                )
            )
        }
    )
    return(invisible(TRUE))
}