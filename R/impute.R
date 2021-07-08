



#' impute
#'
#' TODO - Description
#'
#' @param draws TODO
#' @param data_ice TODO
#' @param references TODO
#' @param strategies TODO
#' @export
impute <- function(draws,  data_ice, references, strategies){
    UseMethod("impute")
}



#' impute.bootstrap
#'
#' TODO - Description
#'
#' @param draws TODO
#' @param data_ice TODO
#' @param references TODO
#' @param strategies TODO
#' @export
impute.bootstrap <- function(draws,  data_ice = NULL, references, strategies = getStrategies()){
    impute_internal(
        draws = draws,
        data_ice = data_ice,
        references = references,
        strategies = strategies,
        conditionalMean = FALSE
    )
}



#' impute.bayesian
#'
#' TODO - Description
#'
#' @param draws TODO
#' @param data_ice TODO
#' @param references TODO
#' @param strategies TODO
#' @export
impute.bayesian <- function(draws,  data_ice = NULL, references, strategies = getStrategies()){
    impute_internal(
        draws = draws,
        data_ice = data_ice,
        references = references,
        strategies = strategies,
        conditionalMean = FALSE
    )
}


#' impute.condmean
#'
#' TODO - Description
#'
#' @param draws TODO
#' @param data_ice TODO
#' @param references TODO
#' @param strategies TODO
#' @export
impute.condmean <- function(draws,  data_ice = NULL, references, strategies = getStrategies()){
    impute_internal(
        draws = draws,
        data_ice = data_ice,
        references = references,
        strategies = strategies,
        conditionalMean = TRUE
    )
}


#' impute_internal
#'
#' TODO - Description
#'
#' @param draws TODO
#' @param data_ice TODO
#' @param references TODO
#' @param strategies TODO
#' @param conditionalMean TODO
impute_internal <- function(draws, data_ice = NULL, references, strategies, conditionalMean = FALSE){

    validate_references(references, draws$longdata)
    validate_strategies(strategies, draws$longdata)

    if(!is.null(data_ice)){
        draws$longdata$update_strategies(data_ice)
    }

    samples_grouped <- transpose_samples(draws$samples)

    imputes <- mapply(
        impute_data_individual,
        names(samples_grouped$index),
        samples_grouped$index,
        MoreArgs = list(
            beta = samples_grouped$beta,
            sigma = samples_grouped$sigma,
            longdata = draws$longdata,
            references = references,
            strategies = strategies,
            conditionalMean = conditionalMean
        ),
        SIMPLIFY = FALSE
    )

    x <- untranspose_samples(imputes, samples_grouped$index)
    return(x)
}


#' transpose_samples
#'
#' TODO - Description
#'
#' @param samples TODO
transpose_samples <- function(samples){

    beta <- list()
    sigma <- list()

    grp_names <- names(samples[[1]]$sigma)
    for( grp in grp_names) {
        sigma[[grp]] <-  vector(mode = "list", length = length(samples))
    }

    for( i in seq_along(samples)){
        sample <- samples[[i]]
        beta[[i]] <- sample$beta
        for( grp in grp_names) sigma[[grp]][[i]] <-  sample$sigma[[grp]]
    }

    index <- invert_indexes( lapply(samples, function(x) x$ids))

    list(
        beta = beta,
        sigma = sigma,
        index = index
    )
}


#' untranspose_samples
#'
#' TODO - Description
#'
#' @param imputes TODO
#' @param indexes TODO
untranspose_samples <- function(imputes, indexes){
    number_of_samples <- max(unlist(indexes))

    HOLD <- list()
    for( i in seq_len(number_of_samples)) HOLD[[i]] <- list()

    for( imp in imputes){
        id <- imp$id
        for( j in seq_along(imp$values)){
            sample_index <- indexes[[id]][[j]]
            hold_index <- length(HOLD[[sample_index]]) + 1
            HOLD[[sample_index]][[hold_index]] <- list(
                id = id,
                values = imp$values[[j]]
            )
        }
    }
    return(HOLD)
}


#' invert_indexes
#'
#' TODO - Description
#'
#' @param x TODO
invert_indexes <- function(x){
    lens <- vapply(x, function(x) length(x), numeric(1))
    grp <- rep(seq_along(x), lens)
    vals <- unlist(x, use.names = FALSE)
    index <- split(grp, vals)
    return(index)
}


#' get_parameters
#'
#' TODO - Description
#'
#' @param id TODO
#' @param index TODO
#' @param beta TODO
#' @param sigma TODO
#' @param longdata TODO
#' @param references TODO
#' @param strategies TODO
#' @param conditionalMean TODO
impute_data_individual <- function(
    id,
    index,
    beta,
    sigma,
    longdata,
    references,
    strategies,
    conditionalMean
){
    result <- list(
        id = id,
        values = replicate(n = length(index), numeric(0))
    )
    values <- longdata$values[[id]]
    if(!any(is.na(values))) return(result)

    index_mar <- longdata$is_mar[[id]]
    strategy <- longdata$strategies[[id]]
    vars <- longdata$vars

    group_pt <- longdata$impgroup[[id]]
    group_ref <- references[group_pt]

    dat_pt <- longdata$get_data(id)

    # Dummy outcome value to stop rows being dropped by model.matrix
    dat_pt[,vars$outcome] <- 1

    # TODO - Enforce group as a factor ?
    dat_ref <- dat_pt
    dat_ref[,vars$group] <- factor(group_ref, levels = levels(group_pt))

    dat_pt_mod <- as_model_df(dat_pt, as_simple_formula(vars))
    dat_ref_mod <- as_model_df(dat_ref, as_simple_formula(vars))

    parameters_group <- get_visit_distribution_parameters(
        dat = dat_pt_mod[-1],
        beta = beta[index],
        sigma = sigma[[group_pt]][index]
    )

    parameters_reference <- get_visit_distribution_parameters(
        dat = dat_ref_mod[-1],
        beta = beta[index],
        sigma = sigma[[group_ref]][index]
    )

    pars <- mapply(
        strategies[[strategy]],
        parameters_group,
        parameters_reference,
        MoreArgs = list(index_mar = index_mar),
        SIMPLIFY = FALSE
    )

    conditional_parameters <- lapply(
        pars,
        get_conditional_parameters,
        values = values
    )

    if(conditionalMean){
        imputed_outcome <- lapply(conditional_parameters, function(x) as.vector(x$mu))
    } else {
        imputed_outcome <- lapply(conditional_parameters, impute_outcome)
    }

    result$values <- imputed_outcome
    return(result)
}


#' as_visit_distribution_parameters
#'
#' TODO - Description
#'
#' @param dat TODO
#' @param beta TODO
#' @param sigma TODO
get_visit_distribution_parameters <- function(dat, beta, sigma){
    beta_mat <- matrix(
        unlist(beta, use.names = FALSE),
        nrow = length(beta[[1]]),
        ncol = length(beta),
        byrow = TRUE
    )
    mu <- as.matrix(dat) %*% beta_mat
    parameters <- list()
    for(i in seq_along(beta)){
        parameters[[i]] <- list(
            mu = mu[,i],
            sigma = sigma[[i]]
        )
    }
    return(parameters)
}


#' impute_outcome
#'
#' TODO - Description
#'
#' @param conditional_parameters TODO
impute_outcome <- function(conditional_parameters){

    if(length(conditional_parameters$mu) == 1){
        result <- rnorm(
            n = 1,
            mean = conditional_parameters$mu,
            sd = conditional_parameters$sigma
        )
    } else {
        result <- mvtnorm::rmvnorm(
            n = 1,
            mean = conditional_parameters$mu,
            sigma = conditional_parameters$sigma,
            method = "chol",
            checkSymmetry = FALSE
        )
    }
    return(as.vector(result))
}


#' get_conditional_parameters
#'
#' TODO - Description
#'
#' @param pars TODO
#' @param values TODO
get_conditional_parameters <- function(pars, values){
    q <- is.na(values)

    if(sum(q) == length(values)) return(pars)
    if(sum(q) == 0) return( list(mu = numeric(0), sigma = numeric(0)))

    a <- values[!q]

    mu1 <- matrix(nrow = sum(q), pars$mu[q])
    mu2 <- matrix(nrow = sum(!q), pars$mu[!q])

    sig11 <- pars$sigma[q,q, drop = FALSE]
    sig12 <- pars$sigma[q,!q, drop = FALSE]
    sig21 <- pars$sigma[!q,q, drop = FALSE]
    sig22 <- pars$sigma[!q,!q, drop = FALSE]

    sig22_inv_12 <-  sig12 %*% solve(sig22)

    list(
        mu = mu1 + sig22_inv_12 %*% (a - mu2),
        sigma = sig11 - sig22_inv_12 %*% sig21
    )

}


#' validate_references
#'
#' TODO - Description
#'
#' @param references TODO
#' @param longdata TODO
validate_references <- function(references, longdata){
    # TODO
}


#' validate_strategies
#'
#' TODO - Description
#'
#' @param strategies TODO
#' @param longdata TODO
validate_strategies <- function(strategies, longdata){
    # TODO
}


