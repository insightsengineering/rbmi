#' Title
#'
#' @name method
#' @param burn_in TODO
#' @param burn_between TODO
#' @param same_cov TODO
#' @param n_imputations TODO
#' @param covariance TODO
#' @param threshold TODO
#' @param REML TODO
#' @return TODO
#' @export
method_bayes <- function(
    burn_in = 200,
    burn_between = 50,
    same_cov = TRUE,
    n_imputations = 20
){
    x <- list(
        burn_in = burn_in,
        burn_between = burn_between,
        same_cov = same_cov,
        n_imputations = n_imputations
    )
    return( as_class(x, "method_bayes"))
}


#' @rdname method
#' @export
method_approxbayes <- function(
    covariance = c("us", "toep", "cs", "ar1"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_imputations = 20
){
    covariance <- match.arg(covariance)

    x <- list(
        covariance = covariance,
        threshold = threshold,
        same_cov = same_cov,
        REML = REML,
        n_imputations = n_imputations
    )
    return( as_class(x, "method_approxbayes"))
}


#' @rdname method
#' @export
method_condmean <- function(
    covariance = c("us", "toep", "cs", "ar1"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_imputations = 20
){
    covariance <- match.arg(covariance)

    x <- list(
        covariance = covariance,
        threshold = threshold,
        same_cov = same_cov,
        REML = REML,
        n_imputations = n_imputations
    )
    return( as_class(x, "method_condmean"))
}


