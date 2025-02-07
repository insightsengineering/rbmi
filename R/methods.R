#' Set the multiple imputation methodology
#'
#' @description
#'
#' These functions determine what methods `rbmi` should use when creating
#' the imputation models, generating imputed values and pooling the results.
#'
#' @name method
#'
#' @param same_cov a logical, if `TRUE` the imputation model will be fitted using a single
#' shared covariance matrix for all observations. If `FALSE` a separate covariance
#' matrix will be fit for each group as determined by the `group` argument of
#' `set_vars()`.
#'
#' @param n_samples a numeric that determines how many imputed datasets are generated.
#' In the case of `method_condmean(type = "jackknife")` this argument
#' must be set to `NULL`. See details.
#' 
#' @param control a list which specifies further lower level details of the computations.
#' Currently only used by `method_bayes()`, please see [control_bayes()] for details and
#' default settings.
#'
#' @param B a numeric that determines the number of bootstrap samples for `method_bmlmi`.
#'
#' @param D a numeric that determines the number of random imputations for each bootstrap sample.
#' Needed for `method_bmlmi()`.
#'
#' @param covariance a character string that specifies the structure of the covariance
#' matrix to be used in the imputation model. Must be one of `"us"` (default), `"ad"`,
#' `"adh"`, `"ar1"`, `"ar1h"`, `"cs"`, `"csh"`, `"toep"`, or `"toeph"`). See details.
#'
#' @param threshold a numeric between 0 and 1, specifies the proportion of bootstrap
#' datasets that can fail to produce valid samples before an error is thrown.
#' See details.
#'
#' @param REML a logical indicating whether to use REML estimation rather than maximum
#' likelihood.
#'
#' @param type a character string that specifies the resampling method used to perform inference
#' when a conditional mean imputation approach (set via `method_condmean()`) is used.
#' Must be one of `"bootstrap"` or `"jackknife"`.
#'
#' @param burn_in deprecated. Please use the `warmup` argument in [control_bayes()] instead.
#'
#' @param burn_between deprecated. Please use the `thin` argument in [control_bayes()] instead.
#'
#' @details
#'
#' In the case of `method_condmean(type = "bootstrap")` there will be `n_samples + 1`
#' imputation models and datasets generated as the first sample will be based on
#' the original dataset whilst the other `n_samples` samples will be
#' bootstrapped datasets. Likewise, for `method_condmean(type = "jackknife")` there will
#' be `length(unique(data$subjid)) + 1` imputation models and datasets generated. In both cases this is
#' represented by `n + 1` being displayed in the print message.
#' In the case that `method_bayes()` is used, and with the `control` argument the number of chains
#' is set to more than 1, then the `n_samples` samples will be distributed across the chains.
#' The total number of returned samples will still be `n_samples`.
#'
#' The user is able to specify different covariance structures using the the `covariance`
#' argument. Currently supported structures include:
#'
#' - Unstructured (`"us"`) (default)
#' - Ante-dependence (`"ad"`)
#' - Heterogeneous ante-dependence (`"adh"`)
#' - First-order auto-regressive (`"ar1"`)
#' - Heterogeneous first-order auto-regressive (`"ar1h"`)
#' - Compound symmetry (`"cs"`)
#' - Heterogeneous compound symmetry (`"csh"`)
#' - Toeplitz (`"toep"`)
#' - Heterogeneous Toeplitz (`"toeph"`)
#'
#' For full details please see [`mmrm::cov_types()`].
#'
#' Note that at present Bayesian methods only support unstructured.
#'
#' In the case of `method_condmean(type = "bootstrap")`, `method_approxbayes()` and `method_bmlmi()` repeated
#' bootstrap samples of the original dataset are taken with an MMRM fitted to each sample.
#' Due to the randomness of these sampled datasets, as well as limitations in the optimisers
#' used to fit the models, it is not uncommon that estimates for a particular dataset can't
#' be generated. In these instances `rbmi` is designed to throw out that bootstrapped dataset
#' and try again with another. However to ensure that these errors are due to chance and
#' not due to some underlying misspecification in the data and/or model a tolerance limit
#' is set on how many samples can be discarded. Once the tolerance limit has been reached
#' an error will be thrown and the process aborted. The tolerance limit is defined as
#' `ceiling(threshold * n_samples)`. Note that for the jackknife method estimates need to be
#' generated for all leave-one-out datasets and as such an error will be thrown if
#' any of them fail to fit.
#'
#' Please note that at the time of writing (September 2021) Stan is unable to produce
#' reproducible samples across different operating systems even when the same seed is used.
#' As such care must be taken when using Stan across different machines. For more information
#' on this limitation please consult the Stan documentation
#' <https://mc-stan.org/docs/2_27/reference-manual/reproducibility-chapter.html>
#'
#' @export
method_bayes <- function(
    same_cov = TRUE,
    n_samples = 20,
    control = control_bayes(),
    burn_in = NULL,
    burn_between = NULL
) {
    assertthat::assert_that(
        is.null(burn_in) && is.null(burn_between),
        msg = paste(
            "The `burn_in` and `burn_between` arguments to `method_bayes()` have been deprecated;",
            "please use the `warmup` and `thin` arguments inside `control_bayes()` instead.",
            collapse = " "
        )
    )

    x <- list(
        same_cov = same_cov,
        n_samples = n_samples,
        control = control
    )
    return(as_class(x, c("method", "bayes")))
}


#' @rdname method
#' @export
method_approxbayes <- function(
    covariance = c("us", "ad", "adh", "ar1", "ar1h", "cs", "csh", "toep", "toeph"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_samples = 20
) {
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
    covariance = c("us", "ad", "adh", "ar1", "ar1h", "cs", "csh", "toep", "toeph"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_samples = NULL,
    type = c("bootstrap", "jackknife")
) {
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
        type = type
    )

    if (type == "bootstrap") {
        x$n_samples <- n_samples
    }

    return(as_class(x, c("method", "condmean")))
}


#' @rdname method
#' @export
method_bmlmi <- function(
    covariance = c("us", "ad", "adh", "ar1", "ar1h", "cs", "csh", "toep", "toeph"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    B = 20,
    D = 2
) {
    covariance <- match.arg(covariance)

    assert_that(
        D > 1,
        msg = "`D` must be a numeric larger than 1"
    )

    x <- list(
        covariance = covariance,
        threshold = threshold,
        same_cov = same_cov,
        REML = REML,
        B = B,
        D = D
    )
    return(as_class(x, c("method", "bmlmi")))
}
