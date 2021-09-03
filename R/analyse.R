



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

    validate(imputations)

    assert_that(
        is.function(fun),
        msg = "`fun` must be a function"
    )

    assert_that(
        is.null(delta) | is.data.frame(delta),
        msg = "`delta` must be NULL or a data.frame"
    )

    vars <- imputations$data$vars

    devnull <- lapply(imputations$imputations, function(x) validate(x))

    if (!is.null(delta)) {
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

    ret <- as_analysis(
        results = results,
        fun_name = capture.output(analysis_call[["fun"]]),
        delta = delta,
        fun = fun,
        method = imputations$method
    )
    validate(ret)
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
) {
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



#' TODO
#' 
#' @param results TODO
#' @param method TODO
#' @param delta TODO
#' @param fun TODO
#' @param fun_name TODO
as_analysis <- function(results, method, delta = NULL, fun = NULL, fun_name = NULL) {

    next_class <- switch(class(method)[[2]],
        bayes = "rubin",
        approxbayes = "rubin",
        condmean = ifelse(
            method$type == "jackknife",
            "jackknife",
            "bootstrap"
        )
    )

    assert_that(
        is.list(results),
        length(next_class) == 1,
        is.character(next_class),
        next_class %in% c("jackknife", "bootstrap", "rubin")
    )

    x <- list(
        results = as_class(results, c(next_class, "list")),
        delta = delta,
        fun = fun,
        fun_name = fun_name,
        method = method
    )
    class(x) <- c("analysis", "list")
    validate(x)
    return(x)
}



#' Print Analysis Object
#'
#' @param x (`analysis`)\cr input
#' @param ... not used
#' @importFrom utils capture.output
#' @export
print.analysis <- function(x, ...) {

    n_samp <- length(x$results)
    n_samp_string <- ife(
        has_class(x$results, "bootstrap") | has_class(x$results, "jackknife"),
        sprintf("1 + %s", n_samp - 1),
        n_samp
    )

    string <- c(
        "",
        "Analysis Object",
        "---------------",
        sprintf("Number of Results: %s", n_samp_string),
        sprintf("Analysis Function: %s", x$fun_name),
        sprintf("Delta Applied: %s", !is.null(x$delta)),
        "Analysis Parameters:",
        sprintf("    %s", names(x$results[[1]])),
        ""
    )

    cat(string, sep = "\n")
    return(invisible(x))
}



#' Validate Analysis Objects
#' 
#' Validates the return object of the analyse() function
#' 
#' @param x A Analysis results object (of class "jackknife", "bootstrap", "rubin")
#' @param ... Not Used
#' @export
validate.analysis <- function(x, ...) {

    next_class <- class(x$results)[[1]]

    assert_that(
        next_class %in% c("jackknife", "bootstrap", "rubin"),
        msg = "`results` must be of class 'jackknife', 'bootstrap' or 'rubin'"
    )

    if (next_class %in% c("bootstrap", "rubin")) {
        nsamp <- ife(
            next_class %in% c("bootstrap"),
            x$method$n_sample + 1,
            x$method$n_sample
        )
        assert_that(
            length(x$results) == nsamp
        )
    }

    assert_that(
        is.list(x$results),
        is.null(x$delta) | is.data.frame(x$delta),
        is.null(x$fun) | is.function(x$fun),
        is.null(x$fun_name) | is.character(x$fun_name)
    )

    validate(x$results)
}


#' @export
validate.jackknife <- function(x, ...) {
    validate_analyse_pars(x, get_pool_components("jackknife"))
}


#' @export
validate.bootstrap <- function(x, ...) {
    validate_analyse_pars(x, get_pool_components("bootstrap"))
}

#' @export
validate.rubin <- function(x, ...) {
    validate_analyse_pars(x, get_pool_components("rubin"))
}


#' TODO
#' 
#' @param results TODO
#' @param pars TODO
#' @export
validate_analyse_pars <- function(results, pars) {

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

    for (par in pars) {
        if (par != "df") {
            assert_that(
                all(!is.na(vapply(results_unnested, function(x) x[[par]], numeric(1)))),
                msg = sprintf("Parameter `%s` contains missing values", par)
            )
        }
    }

    return(invisible(TRUE))
}


