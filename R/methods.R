#' Title
#'
#' @name method
#' @param burn_in TODO
#' @param burn_between TODO
#' @param same_cov TODO
#' @param n_samples TODO
#' @param verbose TODO
#' @return TODO
#' @export
method_bayes <- function(
    burn_in = 200,
    burn_between = 50,
    same_cov = TRUE,
    n_samples = 20,
    verbose = TRUE
){
    x <- list(
        burn_in = burn_in,
        burn_between = burn_between,
        same_cov = same_cov,
        n_samples = n_samples,
        verbose = verbose
    )
    return( as_class(x, "bayes"))
}


#' @rdname method
#' @export
method_approxbayes <- function(
    covariance = c("us", "toep", "cs", "ar1"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_samples = 20
){
    covariance <- match.arg(covariance)

    x <- list(
        covariance = covariance,
        threshold = threshold,
        same_cov = same_cov,
        REML = REML,
        n_samples = n_samples
    )
    return( as_class(x, "approxbayes"))
}


#' @rdname method
#' @export
method_condmean <- function(
    covariance = c("us", "toep", "cs", "ar1"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_samples = NULL,
    type = c("bootstrap", "jackknife")
){
    covariance <- match.arg(covariance)
    type <- match.arg(type)

    if(type == "bootstrap") {
        assert_that(
            !is.null(n_samples),
            msg = "n_samples must not be NULL when type is bootstrap"
        )
    }

    x <- list(
        covariance = covariance,
        threshold = threshold,
        same_cov = same_cov,
        REML = REML,
        n_samples = n_samples,
        type = type
    )
    return( as_class(x, "condmean"))
}


