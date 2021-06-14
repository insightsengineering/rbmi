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

as_model_formula <- function(vars){
    return(list())
}

as_simple_formula <- function(vars){
    variables <- c(
        vars$outcome,
        vars$group,
        vars$visit,
        vars$covariates
    )
    frm <- as.formula(paste0( "~ -1 +", paste0( variables, collapse = " + ")))
    return(frm)
}


as_design <- function(data, frm){
    frame <- model.frame(frm, data = dat, na.action = NULL)
    design_mat <- model.matrix(frm, frame)
    design <- as.data.frame(design_mat)
    return(design)
}


#' @export
draws.method_bootstrap <- function(data, data_ice, vars, method){

    stopifnot(
        is_validate_long(data, vars),
        is_validate_ice(data_ice, vars)
    )
    longdata <- rmDataConstructor$new(data, vars)

    design <- as_design(data, as_simple_formula(vars))
    scaler <- scalerConstructor$new(design)
    initial <- get_mmrm_coefs(
        data = data,
        vars = vars,
        scaler = scaler,
        unscale = FALSE
    )

    samples <- replicate(
        n = method$M,
        {
            get_bootstrap_mmrm_coefs(
                longdata,
                method,
                scaler = scaler,
                initial = initial
            )
        },
        simplify = FALSE
    )

    result <- list(
        method = method,
        data = longdata,
        samples = samples
    )
    return(result)
}


get_bootstrap_mmrm_coefs <- function(longdata, method, ...){
    ids_boot <- longdata$sample_ids()
    dat_boot <- longdata$get_data(ids_boot)

    coefs <- get_mmrm_coefs(dat_boot, longdata$vars, ...)

    result <- list(
        beta = coefs$beta,
        sigma = coef$sigma,
        ids = ids_boot
    )
    return(result)
}


get_mmrm_coefs <- function(data, vars, scaler = NULL, initial = NULL, unscale = TRUE){
    design <- as_design(data, as_simple_formula(vars))
    design_scaled <- scaler$scale(design)

    mmrm <- fit_mmrm(
        data = design_scaled,
        ids = data[[vars$subjid]],
        visits = data[[vars$visits]],
        method = method,
        initialise = initialise
    )

    coefs <- extract_mmrm(mmrm)

    if(unscale){
        sigma <- scaler$unscale_sigma(coefs$sigma)
        beta <- scaler$unscale_sigma(coefs$beta)
    } else {
        sigma <- coefs$sigma
        beta <- coefs$beta
    }


    results <- list(
        beta = beta,
        sigma = sigma
    )

    return(results)
}

draws(iris, iris2, list(), method_bootstrap())





