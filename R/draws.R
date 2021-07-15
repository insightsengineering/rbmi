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
    as_class(x, "approxbayes")
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
        cov_struct = method$cov_struct,
        REML = method$REML,
        same_cov = method$same_cov,
        initial_values = NULL,
        optimizer =  c("L-BFGS-B", "BFGS", "Nelder-Mead")
    )

    initial_sample <- list(
        beta = scaler$unscale_beta(mmrm_initial$beta),
        sigma = scaler$unscale_sigma(mmrm_initial$sigma),
        structure = mmrm_initial$structure,
        subjid = longdata$ids
    )

    init_opt <- list(
        beta = mmrm_initial$beta,
        theta = mmrm_initial$theta
    )

    samples <- append(
        initial_sample,
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
get_bootstrap_samples <- function(longdata,
                                  method,
                                  scaler,
                                  initial = NULL){

    required_samples <- method$n_imputations - 1
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
                sigma = scaler$unscale_sigma(mmrm_fit$sigma),
                structure = mmrm_fit$structure,
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


# #' Title
# #'
# #' @param longdata TODO
# #' @param scaler TODO
# #' @param ... TODO
# get_bootstrap_mmrm_coefs <- function(longdata, scaler = NULL, ...){
#     vars <- longdata$vars
#     ids_boot <- longdata$sample_ids()
#     dat_boot <- longdata$get_data(ids_boot)
#
#     model_df <- as_model_df(dat_boot, as_simple_formula(vars))
#
#     if(!is.null(scaler)){
#         model_df <- scaler$scale(model_df)
#     }
#
#     mmrm <- fit_mmrm(
#         data = model_df[,-1],
#         outcome = model_df[,1],
#         subjid = dat_boot[[vars$subjid]],
#         visit = dat_boot[[vars$visit]],
#         group = dat_boot[[vars$group]],
#         ...
#     )
#
#     result <- list(
#         beta = scaler$unscale_beta(mmrm$beta),
#         sigma = scaler$unscale_sigma(mmrm$sigma),
#         converged = mmrm$converged,
#         structure = mmrm$structure,
#         subjid = ids_boot
#     )
#     return(result)
# }


