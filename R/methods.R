
#' @export
method_bayesian <- function(
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
    return( as_class(x, "method_mcmc"))
}


#' @export
method_bootstrap <- function(
    covariance = c("un", "ar1"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_imputations = 20
){
    x <- list(
        covariance = covariance,
        threshold = threshold,
        same_cov = same_cov,
        REML = REML,
        n_imputations = n_imputations
    )
    return( as_class(x, "method_bootstrap"))
}


#' @export
method_conditionalmean <- function(
    covariance = c("un", "ar1"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_imputations = 20
){
    x <- list(
        covariance = covariance,
        threshold = threshold,
        same_cov = same_cov,
        REML = REML,
        n_imputations = n_imputations
    )
    return( as_class(x, "method_bayesian"))
}


