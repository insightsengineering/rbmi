#' Title
#'
#' @name method
#' @param burn_in TODO
#' @param burn_between TODO
#' @param same_cov TODO
#' @param n_samples TODO
#' @param covariance TODO
#' @param threshold TODO
#' @param REML TODO
#' @param type TODO
#' @param verbose TODO
#' @param seed TODO
#' @return TODO
#' @export
method_bayes <- function(
    burn_in = 200,
    burn_between = 50,
    same_cov = TRUE,
    n_samples = 20,
    verbose = TRUE,
    seed = NA
){
    x <- list(
        burn_in = burn_in,
        burn_between = burn_between,
        same_cov = same_cov,
        n_samples = n_samples,
        verbose = verbose,
        seed = seed
    )
    return(as_class(x, c("method", "bayes")))
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
    return(as_class(x, c("method", "approxbayes")))
}


#' @rdname method
#' @export
method_condmean <- function(
    covariance = c("us", "toep", "cs", "ar1"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_samples = NA,
    type = c("bootstrap", "jackknife")
){
    covariance <- match.arg(covariance)
    type <- match.arg(type)

    if (type == "bootstrap") {
        assert_that(
            is.numeric(n_samples),
            msg = "n_samples must be numeric when type is `bootstrap`"
        )
    }

    if (type == "jackknife") {
        assert_that(
            is.na(n_samples),
            msg = "n_samples must be NA when type is `jackknife`"
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
    return(as_class(x, c("method", "condmean")))
}


