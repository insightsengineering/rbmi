#' Control the computational details of the imputation methods
#' 
#' @description 
#' 
#' These functions control lower level computational details of the imputation methods.
#' 
#' @name control
#' 
#' @param warmup a numeric, the number of warmup iterations for the MCMC sampler.
#' 
#' @param thin a numeric, the thinning rate of the MCMC sampler.
#' 
#' @param chains a numeric, the number of chains to run in parallel.
#' 
#' @param init a character string, the method used to initialise the MCMC sampler, see the details.
#' 
#' @param seed a numeric, the seed used to initialise the MCMC sampler.
#' 
#' @param ... additional arguments to be passed to [rstan::sampling()]. 
#' 
#' @details 
#' 
#' Currently only the Bayesian imputation via [method_bayes()] uses a control function:
#' 
#' - The `init` argument can be set to `"random"` to randomly initialise the sampler with `rstan`
#' default values or to `"mmrm"` to initialise the sampler with the maximum likelihood estimate
#' values of the MMRM.
#' - The `seed` argument is used to set the seed for the MCMC sampler. By default, a random seed
#' is generated, such that outside invocation of the `set.seed()` call can effectively set the 
#' seed.
#' - The samples are split across the chains, such that each chain produces `n_samples / chains`
#' (rounded up) samples. The total number of samples that will be returned across all chains is `n_samples`
#' as specified in [method_bayes()].
#' - Therefore, the additional parameters passed to [rstan::sampling()] must not contain
#' `n_samples` or `iter`. Instead, the number of samples must only be provided directly via the 
#' `n_samples` argument of [method_bayes()]. Similarly, the `refresh` argument is also not allowed 
#' here, instead use the `quiet` argument directly in [draws()].
#'
#' @note For full reproducibility of the imputation results, it is required to use a `set.seed()` call 
#' before defining the `control` list, and calling the `draws()` function. It is not sufficient to
#' merely set the `seed` argument in the `control` list.
#' 
#' @export 
control_bayes <- function( 
    warmup = 200,
    thin = 50,
    chains = 1,
    init = ifelse(chains > 1, "random", "mmrm"),
    seed = sample.int(.Machine$integer.max, 1),
    ...
) {
    additional_pars <- names(list(...))

    if (any(c("n_samples", "iter") %in% additional_pars)) {
        stop(
            "Instead of providing `n_samples` or `iter` here, please specify the",
            " number of samples directly via the `n_samples`",
            " argument of `method_bayes()`"
        )
    }
    if ("refresh" %in% additional_pars) {
        stop(
            "Instead of the `refresh` argument here, please provide the `quiet` argument",
            " directly to `draws()`"
        )
    }
    list(
        warmup = warmup,
        thin = thin,
        chains = chains,
        init = init,
        seed = seed,
        ...
    )
}

complete_control_bayes <- function(
    control,
    n_samples,
    quiet,
    stan_data,
    mmrm_initial
) {
    assertthat::assert_that(is.list(control))
    control_pars <- names(control)
    if ("iter" %in% control_pars) {
        stop("`method$control$iter` must not be specified directly, please use `method$n_samples`")
    }
    assertthat::assert_that(
        assertthat::is.number(control$warmup),
        assertthat::is.number(control$thin),
        assertthat::is.number(control$chains),
        assertthat::is.number(n_samples)
    )
    n_samples_per_chain <- ceiling(n_samples / control$chains)
    control$iter <- control$warmup + control$thin * n_samples_per_chain
    if ("refresh" %in% control_pars) {
        stop("`method$control$refresh` must not be specified directly, please use `quiet`")
    }
    control$refresh <- ife(
        quiet,
        0,
        ceiling(control$iter / 10)
    )
    control$init <- ife(
        identical(control$init, "mmrm"),
        list(list(
            theta = as.vector(stan_data$R %*% mmrm_initial$beta),
            sigma = mmrm_initial$sigma
        )),
        control$init
    )
    if (any(c("object", "data", "pars") %in% control_pars)) {
        stop(
            "The `object`, `data` and `pars` arguments must not be specified",
            " in `method$control`"
        )
    }
    control
}
