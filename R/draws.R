<<<<<<< HEAD
# draws(
#     data = data.frame(...),               # Dataset with all required variables
#     vars = list(
#         visit = “visit”,                 # Factor J-levels
#         group = “arm”,                   # Factor L-levels
#         method = “method”                # Character fixed values i.e. MAR, JTR, etc
#         subjid = “subjid”,               # Character or factor ?
#         outcome = “outcome”,           # Numeric continuous
#         covariates = c(“c1”, “c2”, “c1*c3”, “base_out*visit”)
#     ),
#     data_ice = data.frame(),
#     method = method_obj()                  #  A "method" object
# )
=======
>>>>>>> remotes/origin/develop


#' TODO
#'
#' @name draws
#' @param data TODO
#' @param data_ice TODO
#' @param vars TODO
#' @param method TODO
#'
#' @export
draws <- function(data, data_ice, vars, method){
    UseMethod("draws", method)
}


#' @rdname draws
draws.method_approxbayes <- function(data, data_ice, vars, method){
    x <- draws_bootstrap(data, data_ice, vars, method)
<<<<<<< HEAD
    as_class(x, "approxbayes")
=======

    ### Set ids to be the unique patient values in order
    ### for `impute()` to work as expect for this method (retain
    ### boot_ids for reference)
    x$samples <- lapply(
        x$samples,
        function(x){
            x$ids_boot <- x$ids
            x$ids <- unique(data[[vars$subjid]])
            return(x)
        }
    )
    as_class(x, "bootstrap")
>>>>>>> remotes/origin/develop
}


#' @rdname draws
draws.method_condmean <- function(data, data_ice, vars, method){
    x <- draws_bootstrap(data, data_ice, vars, method)
    as_class(x, "condmean")
}


#' @rdname draws
draws_bootstrap <- function(data, data_ice, vars, method){

    longdata <- longDataConstructor$new(data, vars)

    model_df <- as_model_df(data, as_simple_formula(vars))
    scaler <- scalerConstructor$new(model_df)

    model_df_scaled <- scaler$scale(model_df)

    mmrm_initial <- fit_mmrm_multiopt(
        designmat = model_df_scaled[,-1],
        outcome = model_df_scaled[,1],
        subjid = data[[vars$subjid]],
        visit = data[[vars$visit]],
        group = data[[vars$group]],
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
        structure = mmrm_initial$structure,
        converged = mmrm_initial$converged,
        optimizer = mmrm_initial$optimizer,
        subjid = longdata$ids
    )

    init_opt <- list(
        beta = mmrm_initial$beta,
        theta = mmrm_initial$theta
    )

    samples <- append(
        list(initial_sample),
        get_bootstrap_samples(
            longdata = longdata,
            method = method,
            scaler = scaler,
            initial = init_opt
        )
    )

    structures <- lapply(
        samples,
        function(x) x[["structure"]]
    )

    optimizers <- lapply(
        samples,
        function(x) x[["optimizer"]]
    )

    result <- list(
        method = method,
        data = longdata,
        samples = samples,
        structures = structures,
        optimizers = optimizers
    )

    return(result)
}


#' Title
#'
#' @param ... TODO
#' @param method TODO
<<<<<<< HEAD
get_bootstrap_samples <- function(longdata,
                                  method,
                                  scaler,
                                  initial = NULL){

    required_samples <- method$n_imputations - 1
=======
get_bootstrap_samples <- function(method, ...){
    required_samples <- method$M - 1 # -1 as the first sample is done in advance on the full dataset
>>>>>>> remotes/origin/develop
    samples <- vector("list", length = required_samples)
    current_sample <- 1
    failed_samples <- 0
    failure_limit <- ceiling(method$threshold * required_samples)

    while(current_sample <= required_samples & failed_samples <= failure_limit){

        # create bootstrapped sample
        vars <- longdata$vars
        ids_boot <- longdata$sample_ids()
        dat_boot <- longdata$get_data(ids_boot)

        model_df <- as_model_df(dat_boot, as_simple_formula(vars))
        model_df_scaled <- scaler$scale(model_df)

        # fit mmrm
        mmrm_fit <- fit_mmrm_multiopt(
            designmat = model_df_scaled[,-1],
            outcome = model_df_scaled[,1],
            subjid = data[[vars$subjid]],
            visit = data[[vars$visit]],
            group = data[[vars$group]],
            vars = vars,
            cov_struct = method$covariance,
            REML = method$REML,
            same_cov = method$same_cov,
            initial_values = initial,
            optimizer = c("BFGS", "L-BFGS-B", "Nelder-Mead")
        )

        if(mmrm_fit$converged){
            sample <- list(
                beta = scaler$unscale_beta(mmrm_fit$beta),
                sigma = lapply(mmrm_fit$sigma, scaler$unscale_sigma),
                structure = mmrm_fit$structure,
                converged = mmrm_fit$converged,
                optimizer = mmrm_fit$optimizer,
                subjid = ids_boot
            )

            samples[[current_sample]] <- sample
            current_sample <- current_sample + 1

        } else {
            failed_samples <- failed_samples + 1
        }

    }
    return(samples)
}
<<<<<<< HEAD
=======

>>>>>>> remotes/origin/develop
