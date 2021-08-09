#' TODO
#'
#' @name draws
#' @param data TODO
#' @param data_ice TODO
#' @param vars TODO
#' @param method TODO
#'
#' @export
draws <- function(data, data_ice, vars, method) {
    UseMethod("draws", method)
}

#' @rdname draws
#' @export
draws.bayes <- function(data, data_ice, vars, method) {
    x <- draws_bayes(data, data_ice, vars, method)
    as_class(x, "random")
}

#' @rdname draws
#' @export
draws.approxbayes <- function(data, data_ice, vars, method) {

    method$type <- "bootstrap" # just for internal use

    x <- draws_bootstrap(data, data_ice, vars, method)

    ### Set ids to be the unique patient values in order
    ### for `impute()` to work as expect for this method (retain
    ### boot_ids for reference)
    x$samples <- lapply(
        x$samples,
        function(x){
            x$ids <- unique(data[[vars$subjid]])
            return(x)
        }
    )

    # remove useless elements from output of `method`
    x$method$type <- NULL

    as_class(x, "random")
}

#' @rdname draws
#' @export
draws.condmean <- function(data, data_ice, vars, method) {
    x <- draws_bootstrap(data, data_ice, vars, method)
    as_class(x, "condmean")
}

#' Title - TODO
#'
#' @param longdata TODO
#' @param outcome_var TODO
remove_nmar_as_NA <- function(longdata, outcome_var) {

    # remove non-MAR data
    data <- longdata$get_data(longdata$ids, nmar.rm = FALSE, na.rm = FALSE)
    isMAR <- unlist(longdata$is_mar)
    data[!isMAR, outcome_var] <- NA

    return(data)
}

#' Title - TODO
#'
#' @param data TODO
#' @param data_ice TODO
#' @param vars TODO
#' @param method TODO
#' @importFrom stats setNames
draws_bayes <- function(data, data_ice, vars, method) {

    longdata <- longDataConstructor$new(data, vars)
    longdata$set_strategies(data_ice)

    data2 <- remove_nmar_as_NA(longdata, vars$outcome)

    # compute design matrix
    model_df <- as_model_df(data2, as_simple_formula(vars))

    # scale input data
    scaler <- scalerConstructor$new(model_df)
    model_df_scaled <- scaler$scale(model_df)

    # fit MMRM (needed for initial values)
    mmrm_initial <- fit_mmrm_multiopt(
        designmat = model_df_scaled[,-1],
        outcome = model_df_scaled[,1],
        subjid = data2[[vars$subjid]],
        visit = data2[[vars$visit]],
        group = data2[[vars$group]],
        vars = vars,
        cov_struct = "us",
        REML = TRUE,
        same_cov = method$same_cov,
        initial_values = NULL,
        optimizer =  c("L-BFGS-B", "BFGS", "Nelder-Mead")
    )

    # run MCMC
    fit <- run_mcmc(
        designmat = model_df_scaled[, -1],
        outcome = model_df_scaled[, 1, drop = TRUE],
        group = data2[[vars$group]],
        sigma_reml = mmrm_initial$sigma,
        n_imputations = method$n_samples,
        burn_in = method$burn_in,
        burn_between = method$burn_between,
        initial_values = list(
            beta = mmrm_initial$beta,
            sigma = mmrm_initial$sigma
        ),
        same_cov = method$same_cov
    )

    # set names of covariance matrices
    fit$samples$sigma <- lapply(
        fit$samples$sigma,
        function(sample_cov) setNames(sample_cov, levels(data2[[vars$group]]))
    )

    # unscale samples
    samples <- mapply(function(x,y) list("beta" = x, "sigma" = y),
                      lapply(fit$samples$beta, scaler$unscale_beta),
                      lapply(fit$samples$sigma, function(covs) lapply(covs, scaler$unscale_sigma)),
                      SIMPLIFY = FALSE
    )

    # set ids associated to each sample
    samples <- lapply(
        samples,
        function(x) {x$ids <- longdata$ids; return(x)}
    )

    result <- list(
        method = method,
        samples = samples,
        data = longdata,
        fit = fit$fit
    )

    return(result)
}

#' Title
#'
#' @param data TODO
#' @param data_ice TODO
#' @param vars TODO
#' @param method TODO
draws_bootstrap <- function(data, data_ice, vars, method) {

    longdata <- longDataConstructor$new(data, vars)
    longdata$set_strategies(data_ice)

    data2 <- longdata$get_data(longdata$ids, nmar.rm = TRUE, na.rm = TRUE)
    model_df <- as_model_df(data2, as_simple_formula(vars))

    scaler <- scalerConstructor$new(model_df)
    model_df_scaled <- scaler$scale(model_df)

    mmrm_initial <- fit_mmrm_multiopt(
        designmat = model_df_scaled[, -1],
        outcome = model_df_scaled[, 1],
        subjid = data2[[vars$subjid]],
        visit = data2[[vars$visit]],
        group = data2[[vars$group]],
        vars = vars,
        cov_struct = method$covariance,
        REML = method$REML,
        same_cov = method$same_cov,
        initial_values = NULL,
        optimizer =  c("L-BFGS-B", "BFGS", "Nelder-Mead")
    )

    initial_sample <- list(
        beta = scaler$unscale_beta(mmrm_initial$beta),
        sigma = lapply(mmrm_initial$sigma, scaler$unscale_sigma),
        converged = mmrm_initial$converged,
        optimizer = mmrm_initial$optimizer,
        ids_boot = longdata$ids
    )

    init_opt <- list(
        beta = mmrm_initial$beta,
        theta = mmrm_initial$theta
    )

    if(method$type == "bootstrap") {
        samples <- get_bootstrap_samples(
            longdata = longdata,
            method = method,
            scaler = scaler,
            initial = init_opt
        )

    } else if(method$type == "jackknife") {
        samples <- get_jackknife_samples(
            longdata = longdata,
            method = method,
            scaler = scaler,
            initial = init_opt
        )
    }

    n_failures <- samples$n_failures

    samples <- append(
        list(initial_sample),
        samples$samples
    )

    optimizers <- lapply(
        samples,
        function(x) x[["optimizer"]]
    )

    result <- list(
        method = method,
        data = longdata,
        samples = samples,
        optimizers = optimizers,
        n_failures = n_failures
    )

    return(result)
}


#' Title - TODO
#'
#' @param longdata TODO
#' @param method TODO
#' @param scaler TODO
#' @param initial TODO
get_bootstrap_samples <- function(
    longdata,
    method,
    scaler,
    initial = NULL
){

    vars <- longdata$vars

    required_samples <- method$n_samples - 1 # -1 as the first sample is done in advance on the full dataset
    samples <- vector("list", length = required_samples)
    current_sample <- 1
    failed_samples <- 0
    failure_limit <- ceiling(method$threshold * required_samples)

    while(current_sample <= required_samples & failed_samples <= failure_limit){

        # create bootstrapped sample
        ids_boot <- longdata$sample_ids()
        dat_boot <- longdata$get_data(ids_boot, nmar.rm = TRUE, na.rm = TRUE)

        model_df <- as_model_df(dat_boot, as_simple_formula(vars))
        model_df_scaled <- scaler$scale(model_df)

        # fit mmrm
        mmrm_fit <- fit_mmrm_multiopt(
            designmat = model_df_scaled[,-1],
            outcome = model_df_scaled[,1],
            subjid = dat_boot[[vars$subjid]],
            visit = dat_boot[[vars$visit]],
            group = dat_boot[[vars$group]],
            vars = vars,
            cov_struct = method$covariance,
            REML = method$REML,
            same_cov = method$same_cov,
            initial_values = initial,
            optimizer = c("L-BFGS-B", "BFGS", "Nelder-Mead")
        )

        if(mmrm_fit$converged){
            sample <- list(
                beta = scaler$unscale_beta(mmrm_fit$beta),
                sigma = lapply(mmrm_fit$sigma, scaler$unscale_sigma),
                converged = mmrm_fit$converged,
                optimizer = mmrm_fit$optimizer,
                ids_boot = ids_boot
            )

            samples[[current_sample]] <- sample
            current_sample <- current_sample + 1

        } else {
            failed_samples <- failed_samples + 1
        }

    }

    if(failed_samples > failure_limit) {
        stop(paste0("More than ", failure_limit, " failed fits. Increase the failures threshold or set a different covariance structure"))
    }

    return(
        list(
            samples = samples,
            n_failures = failed_samples
        )
    )
}

#' Title
#'
#' @param longdata TODO
#' @param method TODO
#' @param scaler TODO
#' @param initial TODO
get_jackknife_samples <- function(
    longdata,
    method,
    scaler,
    initial = NULL
){

    vars <- longdata$vars
    ids <- longdata$ids

    required_samples <- length(ids)
    samples <- vector("list", length = required_samples)
    current_sample <- 1
    failed_samples <- 0
    failure_limit <- ceiling(method$threshold * required_samples)

    while(current_sample <= required_samples & failed_samples <= failure_limit){

        ids_boot <- ids[-current_sample]
        dat_boot <- longdata$get_data(ids_boot, nmar.rm = TRUE, na.rm = TRUE)

        model_df <- as_model_df(dat_boot, as_simple_formula(vars))
        model_df_scaled <- scaler$scale(model_df)

        # fit mmrm
        mmrm_fit <- fit_mmrm_multiopt(
            designmat = model_df_scaled[,-1],
            outcome = model_df_scaled[,1],
            subjid = dat_boot[[vars$subjid]],
            visit = dat_boot[[vars$visit]],
            group = dat_boot[[vars$group]],
            vars = vars,
            cov_struct = method$covariance,
            REML = method$REML,
            same_cov = method$same_cov,
            initial_values = initial,
            optimizer = c("L-BFGS-B", "BFGS", "Nelder-Mead")
        )

        sample <- list(
            beta = scaler$unscale_beta(mmrm_fit$beta),
            sigma = lapply(mmrm_fit$sigma, scaler$unscale_sigma),
            converged = mmrm_fit$converged,
            optimizer = mmrm_fit$optimizer,
            ids_boot = ids_boot
        )

        samples[[current_sample]] <- sample
        current_sample <- current_sample + 1

        if(!mmrm_fit$converged) {
            failed_samples <- failed_samples + 1
        }

    }

    if(failed_samples > failure_limit) {
        stop(paste0("More than ", failure_limit, " failed fits. Increase the failures threshold or set a different covariance structure"))
    }

    return(
        list(
            samples = samples,
            n_failures = failed_samples
        )
    )
}
