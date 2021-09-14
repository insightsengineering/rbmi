#' Imputation & analysis Methods
#'
#' @description
#'
#' These functions determine what methods rbmi should use when creating
#' the imputation models, generating imputed values and pooling the results.
#'
#' @name method
#'
#' @param burn_in a numeric that specifies how many observations should be discarded
#' prior to extracting actual samples. Note that the sampler
#' is initialised at the maximum likelihood estimates and a weakly informative
#' prior is used thus in theory this value should not need to be that high.
#'
#' @param burn_between a numeric that specifies the "thinning" rate i.e. how many
#' observations should be discarded between each sample. This is used to prevent
#' issues associated with autocorrelation between samples.
#'
#' @param same_cov a logical, if `TRUE` the MMRM model will be fitted using a single
#' shared covariance matrix for all observations. If `FALSE` a separate covariance
#' matrix will be fit for each group as determined by the `group` argument of
#' `set_vars()`.
#'
#' @param n_samples a numeric that determines how many imputed datasets are generated.
#' In the case of `method_condmean(type = "jackknife")` this argument
#' must be set to `NULL`. See details.
#'
#' @param covariance a character string that specifies the structure of the covariance
#' matrix to be used in the imputation model. Must be one of `"us"` (default), `"toep"`,
#' `"cs"` or `"ar1"`. See details.
#'
#' @param threshold a numeric between 0 and 1, specifies the proportion of bootstrap
#' datasets that can fail to produce valid samples before an error is thrown.
#' See details.
#'
#' @param REML a logical indicating whether to use REML estimation rather than maximum
#' likelihood.
#'
#' @param type a character string that specifies the method to use when calculating
#' confidence intervals. Must be one of `"bootstrap"` (default) or `"percentile"`
#'
#' @param verbose a logical, if `TRUE` (default) Stan's sampling log information will
#' be printed to the console, if `FALSE` this information will be suppressed.
#'
#' @param seed a numeric that specifies a seed to be used in the call to Stan. This
#' argument is forward on the the `seed` argument of [rstan::sampling()]. Note that
#' this is only required for `method_bayes()`, for all other methods you can achieve
#' reproducible results by setting the seed via `set.seed()`
#'
#' @details
#'
#' In the case of `method_condmean(type = "bootstrap")` there will be `n_samples + 1`
#' imputation models and datasets generated as the first sample will be based on
#' the original dataset whilst the other `n_samples` samples will be
#' bootstrapped datasets. Likewise, for `method_condmean(type = "jackknife")` there will
#' be `nrow(data) + 1` imputation models and datasets generated. In both cases this is
#' represented by `n + 1` being displayed in the print message.
#'
#' The user is able to specify different covariance structures using the the `covariance`
#' argument. Currently supported structures include:
#'
#' - Unstructured (`"us"`)
#' - Toeplitz (`"toep"`)
#' - Compound Symmetry (`"cs"`)
#' - Autoregression-1 (`"ar1"`)
#'
#' Note that at present Bayesian methods only support unstructured.
#'
#' In the case of `method_condmean(type = "bootstrap")` and `method_approxbayes()`, repeated
#' bootstrap samples of the original dataset are taken with an MMRM fitted to each sample.
#' Due to the randomness of these sampled datasets, as well as limitations in the optimisers
#' used to fit the models, it is not uncommon that estimates for a particular dataset can't
#' be generated. In these instances rbmi is designed to throw out that bootstrapped data
#' and try again with another. However to ensure that these errors are due to chance and
#' not due to some underlying misspecification in the data and/or model a tolerance limit
#' is set on how many samples can be discarded. Once the tolerance limit has been reached
#' an error will be thrown and the process aborted. The tolerance limit is defined as
#' `ceiling(threshold * n_samples)`. Note that for the jackknife method estimates need to be
#' generated for all leave-one-out datasets and as such an error will be thrown if
#' any of them fail to fit.
#'
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
    n_samples = NULL,
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
            is.null(n_samples),
            msg = "n_samples must be NULL when type is `jackknife`"
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


