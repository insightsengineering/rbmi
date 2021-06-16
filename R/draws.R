# draws(
#     data = data.frame(...),               # Dataset with all required variables
#     vars = list(
#         visit = “visit”,                 # Factor J-levels
#         group = “arm”,                   # Factor L-levels
#         method = “method”                # Character fixed values i.e. MAR, JTR, etc
#         subjid = “subjid”,               # Character or factor ?
#         response = “response”,           # Numeric continuous
#         covariates = c(“c1”, “c2”, “c1*c3”, “base_out*visit”)
#     ),
#     data_ice = data.frame(),
#     method = method_obj()                  #  A "method" object
# )


#' @export
draws <- function(data, data_ice, vars, method){
    UseMethod("draws", method)
}


#' @export
draws.method_bootstrap <- function(data, data_ice, vars, method){
    x <- draws_bootstrap(data, data_ice, vars, method)
    as_class(x, "bootstrap")
}


#' @export
draws.method_condmean <- function(data, data_ice, vars, method){
    x <- draws_bootstrap(data, data_ice, vars, method)
    as_class(x, "condmean")
}


draws_bootstrap <- function(data, data_ice, vars, method){

    stopifnot(
        is_valid_long(data, vars),
        is_valid_ice(data_ice, vars)
    )
    longdata <- rmDataConstructor$new(data, vars)

    model_df <- as_model_df(data, as_simple_formula(vars))
    scaler <- scalerConstructor$new(model_df)

    model_df_scaled <- scaler$scale(model_df)

    mmrm_initial <- fit_mmrm(
        data = model_df_scaled[,-1],
        outcome = model_df_scaled[,1],
        ids = data[[vars$subjid]],
        visits = data[[vars$visits]],
        groups = data[[vars$groups]],
        method = method
    )

    iniital_sample <- list(
        beta = scaler$unscale_beta(mmrm_initial$beta),
        sigma = scaler$unscale_sigma(mmrm_initial$sigma),
        structure = mmrm_initial$structure,
        ids = longdata$ids
    )

    samples <- append(
        initial_sample,
        get_bootstrap_samples(
            longdata = longdata,
            method = method,
            scaler = scaler,
            initial = iniital_sample
        )
    )

    structures <- lappy(
        samples,
        function(x) x[["structure"]]
    )

    ## TODO - Code to summarise structures

    result <- list(
        method = method,
        data = longdata,
        samples = samples,
        structures = structures
    )

    return(result)
}


get_bootstrap_samples <- function(method, ...){
    required_samples <- method$M - 1
    samples <- vector("list", length = required_samples)
    current_sample <- 1
    failed_samples <- 0
    failure_limit <- ceiling(method$threshold * required_samples)

    while(current_sample <= required_samples & failed_samples <= failure_limit){
        sample <- get_bootstrap_mmrm_coefs(
            longdata = longdata,
            method = method,
            ...
        )

        if(sample$converged){
            samples[[current_sample]] <- sample
            current_sample <- current_sample + 1
        }

        if( !sample$converged | sample$structure != method$structure[[1]]){
            failed_samples <- failed_samples + 1
        }
    }
    return(samples)
}


get_bootstrap_mmrm_coefs <- function(longdata, scaler = NULL, ...){
    vars <- longdata$vars
    ids_boot <- longdata$sample_ids()
    dat_boot <- longdata$get_data(ids_boot)

    model_df <- as_model_df(dat_boot, as_simple_formula(vars))

    if(!is.null(scaler)){
        model_df <- scaler$scale(model_df)
    }

    mmrm <- fit_mmrm(
        data = model_df[,-1],
        outcome = model_df[,1],
        ids = dat_boot[[vars$subjid]],
        visits = dat_boot[[vars$visits]],
        groups = dat_boot[[vars$groups]],
        ...
    )

    result <- list(
        beta = scaler$unscale_beta(mmrm$beta),
        sigma = scaler$unscale_sigma(mmrm$sigma),
        converged = mmrm$converged,
        structure = mmrm$structure,
        ids = ids_boot
    )
    return(result)
}


