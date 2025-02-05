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
#' is generated, such that outside invocation of the `set.seed()` call will effectively set the 
#' seed.
#' - The additional parameters passed to [rstan::sampling()] must not contain `n_samples` or `iter`.
#' Instead, the number of samples must be provided directly via the `n_samples` argument of
#' [method_bayes()]. The `refresh` argument is also not allowed here, instead use the `quiet` argument
#' directly in [draws()].
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
    additional <- list(...)
    additional_pars <- names(additional)
    
    if (any(c("n_samples", "iter") %in% additional_pars)) {
        stop(
            "Please provide the number of samples directly via the `n_samples`",
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
        additional = list(...)
    )
}
