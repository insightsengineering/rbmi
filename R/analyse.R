



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

    analysis_call <- match.call()

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
        function(x, ...) {
            dat2 <- extract_imputed_df(x, imputations$data, delta)
            fun(dat2, ...)
        },
        ...
    )

    ret <- list(
        results = results,
        call = analysis_call,
        delta = delta,
        fun = fun
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

    class(ret$results) <- new_class
    class(ret) <- c(new_class, "analysis")
    validate_analyse(ret$results)
    return(ret)
}



#' Extract imputated datasets
#'
#' @description
#' TODO
#'
#' @param imputations TODO
#' @param index TODO
#' @param delta TODO
#' @param idmap TODO
#' 
#' @export
extract_imputed_dfs <- function(
    imputations,
    index = seq_along(imputations$imputations),
    delta = NULL,
    idmap = FALSE
){
    x <- imputations$imputations[index]
    lapply(
        x,
        function(x) extract_imputed_df(x, imputations$data, delta, idmap)
    )
}


#' Extract imputated dataset
#'
#' @description
#' TODO
#'
#' @param imputation TODO
#' @param ld TODO
#' @param delta TODO
#' @param idmap TODO
extract_imputed_df <- function(imputation, ld, delta = NULL, idmap = FALSE) {
    vars <- ld$vars
    dat <- ld$get_data(imputation, idmap = TRUE)
    id_map <- attr(dat, "idmap")

    if (!is.null(delta)) {
        # We are injecting a variable into the dataset so are using a obscured variable
        # name to remove the chance of a clash
        oldvar <- "old_subject_variable_zkfed1fgkadwni6g4oajd2aw"
        dat[[oldvar]] <- id_map[dat[[vars$subjid]]]
        delta[[oldvar]] <- delta[[vars$subjid]]
        dat2 <- apply_delta(
            dat,
            delta,
            group = c(oldvar, vars$visit),
            outcome = vars$outcome
        )
        dat2[[oldvar]] <- NULL
    } else {
        dat2 <- dat
    }

    if (idmap) {
        attr(dat2, "idmap") <- id_map
    } else {
        attr(dat2, "idmap") <- NULL
    }

    return(dat2)
}



#' Print Analysis Object
#'
#' @param x (`analysis`)\cr input
#' @param ... not used
#' @export
print.analysis <- function(x, ...) {

    string <- c(
        "",
        "Analysis Object",
        "---------------",
        sprintf("Number of Results: %s", length(x$results)),
        sprintf("Analysis Function: %s", capture.output(x$call[["fun"]])),
        sprintf("Delta Applied: %s", !is.null(x$delta)),
        "Analysis Parameters:",
        sprintf("    %s", names(x$results[[1]])),
        ""
    )

    cat(string, sep = "\n")
    return(invisible(x))
}

