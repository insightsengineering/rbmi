
#' Title
#'
#' @param x TODO
#' @param cls TODO
#' @export
as_class <- function(x, cls){
    class(x) <- cls
    return(x)
}



#' Title
#'
#' @param vars TODO
as_simple_formula <- function(vars){
    variables <- c(
        vars$group,
        vars$visit,
        vars$covariates
    )
    frm <- stats::as.formula(
        paste0(
            vars$outcome,
            "~ 1 + ",
            paste0( variables, collapse = " + " )
        )
    )
    return(frm)
}


#' Expand dataframe into a design matrix
#'
#' Expands out a dataframe using a formula to create a design matrix.
#' Key details are that it will always place the outcome variable into
#' the first column of the return object.
#'
#' The outcome column may contain NA's but none of the other variables
#' listed in the formula should contain missing values
#'
#' @param dat a data.frame
#' @param frm a formula
as_model_df <- function(dat, frm){

    outcome <- as.character(attr(stats::terms(frm), "variables")[[2]])
    is_missing <- is.na(dat[[outcome]])
    dat[[outcome]][is_missing] <- 999
    design_mat <- stats::model.matrix(frm, dat)
    dat[[outcome]][is_missing] <- NA

    assert_that(
        nrow(design_mat) == nrow(dat),
        msg = "Model matrix has less rows than input dataset. You may have missing values."
    )
    full_mat <- cbind(dat[[outcome]], design_mat)
    colnames(full_mat)[[1]] <- outcome
    design <- as.data.frame(full_mat)
    class(design) <- class(dat)
    return(design)
}


#' Title - TODO
#'
#' Converts all character variables within a dataframe to factor
#'
#' @param data A dataframe
char2fct <- function(data) {
    for (v in colnames(data)) {
        if (is.character(data[[v]])) {
            data[[v]] <- factor(data[[v]])
        }
    }
    return(data)
}


#' Title - TODO
#'
#' A wrapper around if() else() to prevent unexpected
#' interactions between ifelse() and factor variables
#'
#' @param x True / False
#' @param a value to return if True
#' @param b value to return if False
ife <- function(x, a, b) {
    if (x) {
        return(a)
    } else {
        return(b)
    }
}





#' Sample random values from the multivariate normal distribution
#'
#' @param mu mean vector
#' @param sigma covariance matrix
#'
#' Samples multivariate normal variables by multiplying
#' univariate random normal variables by the cholesky
#' decomposition of the covariance matrix.
#'
#' If mu is length 1 then just uses rnorm instead.
sample_mvnorm <- function(mu, sigma) {
    if (length(sigma) == 1 & length(mu) == 1) {
        return(rnorm(1, mu, sqrt(sigma)))
    }
    assert_that(
        is.matrix(sigma),
        nrow(sigma) == ncol(sigma),
        nrow(sigma) == length(mu),
        msg = "`mu` and `sigma` are not of compatible sizes"
    )
    x <- rnorm(nrow(sigma), mean = 0, sd = 1)
    (x %*% chol(sigma)) + as.vector(mu)
}




#' Capture Warnings
#'
#' This function silences all warnings and instead returns
#' a list with elements `result` containing the output of the function
#' and `warning` a vector containing all warnings that were raised
#'
#' @param expr An expression to be executed
record_warnings <- function(expr) {
    env <- new.env()
    env$warning <- NULL

    result <- withCallingHandlers(expr, warning = function(w) {
        env$warning <- c(env$warning, w$message)
        invokeRestart("muffleWarning")
    })

    list(results = result, warnings = env$warning)
}

#' Is value absent
#'
#' Returns true if a value is either NULL, NA or "".
#' In the case of a vector all values must be NULL/NA/""
#' for x to be regarded as absent.
#'
#' @param x a value to check if it is absent or not
#' @param na do NAs count as absent
#' @param blank do blanks i.e. "" count as absent
is_absent <- function(x, na = TRUE, blank = TRUE) {
    if (is.null(x)) {
        return(TRUE)
    }


    if (na) {
        if (all(is.na(x))) {
            return(TRUE)
        }
    }

    if (blank & is.character(x)) {
        if (all(x == "")) {
            return(TRUE)
        }
    }

    return(FALSE)
}




#' Extract Variables from string vector
#'
#' Takes a string including potentially model terms like `*` and `:` and
#' extracts out the individual variables
#'
#' i.e.  `c("v1", "v2", "v2*v3", "v1:v2")` becomes `c("v1", "v2", "v3")`
#'
#' @param x string of variable names potentially including interaction terms
extract_covariates <- function(x) {
    if(is.null(x)) return(x)
    x_split <- strsplit(x, ":|\\*")
    x_vec <- unlist(x_split, use.names = FALSE)
    x_nws <- trimws(x_vec)
    x_uni <- unique(x_nws)
    return(x_uni)
}
