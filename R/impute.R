

#' @export
impute <- function(draws,  data_ice, references, strategies = strategies()){
    UseMethod("impute", draws,  data_ice, references, strategies)
}


impute.bootstrap <- function(draws,  data_ice = NULL, references, strategies){

    validate_references(references, draws$longdata)
    validate_strategies(strategies, draws$longdata)

    if(!is.null(data_ice)){
        draws$longdata$update_strategies(data_ice)
    }

    imputes <- lapply(
        X = draws$samples,
        FUN = impute_data,
        longdata = draws$longdata,
        references = references,
        strategies = strategies
    )
    return(imputes)
}


impute_data <- function(sample, ...){
    lapply(
        X = sample$ids,
        FUN = impute_data_individual,
        beta = sample$beta,
        sigma = sample$sigma,
        ...
    )
}


impute_data_individual <- function(
    id,
    beta,
    sigma,
    longdata,
    references,
    strategies
){
    result <- list(
        id = id,
        values = numeric(0)
    )
    values <- longdata$values[[id]]
    if(!any(is.na(values))) return(result)

    index_mar <- longdata$is_mar[[id]]
    strategy <- longdata$strategies[[id]]
    vars <- longdata$vars

    group_pt <- longdata$impgroup[[id]]
    group_ref <- references[group_pt]

    dat_pt <- longdata$get_data(id)
    dat_pt[,vars$outcome] <- 1
    dat_ref <- dat_pt
    dat_ref[,vars$group] <- factor(group_ref, levels = levels(group_pt))

    dat_pt_mod <- as_model_df(dat_pt, as_simple_formula(vars))
    dat_ref_mod <- as_model_df(dat_ref, as_simple_formula(vars))

    parameters_group <- list(
        mu = get_mu(dat_pt_mod[,-1], beta),
        sigma = sigma[[group_pt]]
    )

    parameters_reference <- list(
        mu = get_mu(dat_ref_mod[,-1], beta),
        sigma = sigma[[group_ref]]
    )

    pars <- strategies[[strategy]](
        parameters_group,
        parameters_reference,
        index_mar = index_mar
    )

    conditional_parameters <- get_conditional_parameters(pars, values)
    imputed_outcome <- impute_outcome(conditional_parameters)

    result$values <- imputed_outcome
    return(result)
}


impute_outcome <- function(conditional_parameters){
    mvtnorm::rmvnorm(
        n = 1,
        mean = conditional_parameters$condMean,
        sigma = conditional_parameters$condVar
    )
}


# condMVNorm::condMVN(
#     mean = c(1,2),
#     sigma = as_covmat(c(1,2), c(0.9, 0.5)) ,
#     dependent.ind = c(2),
#     given.ind = c(1),
#     X.given = c(5)
# )

get_conditional_parameters <- function(pars, values){

    x <- condMVNorm::condMVN(
        mean = pars$mu,
        sigma = pars$sigma,
        dependent.ind = which(is.na(values)),
        given.ind = which(!is.na(values)),
        X.given	= values[!is.na(values)]
    )

    return(x)
    # is_miss <- is.na(values)
    # mu1 <- pars$mu[is_miss]
    # mu2 <- pars$mu[!is_miss]
    # sig11 <- pars$sigma[is_miss,is_miss]
    # sig12 <- pars$sigma[is_miss,!is_miss]
    # sig21 <- pars$sigma[!is_miss,is_miss]
    # sig22 <- pars$sigma[!is_miss,!is_miss]
    # a <- values[!is_miss]
    # cond_pars <- list(
    #     mu = mu1 + (sig12 %*% solve(sig22)) %*% (a - mu2),
    #     sigma = sig11 - (sig12 %*% solve(sig22)) %*% sig21
    # )
    return(cond_pars)
}



get_mu <- function(dat, beta){
    as.matrix(dat) %*% beta
}

validate_references <- function(references, longdata){
    # TODO
}

validate_strategies <- function(strategies, longdata){
    # TODO
}

strategies <- function(...){
    user_strats <- list(...)
    pkg_strats <- list(
        "JR" = strategy_JR,
        "CR" = strategy_CR,
        "CIR" = strategy_CIR,
        "LMCF" = strategy_LMCF
    )
    for(i in names(user_strats)) {
        stopifnot(
            is.function(user_strats[[i]])
        )
        pkg_strats[[i]] <- user_strats[[i]]
    }
    pkg_strats[["MAR"]] <- strategy_MAR
    return(pkg_strats)
}


strategy_MAR <- function(parameters_group, parameters_reference, index_mar){
    return(parameters_group)
}

strategy_JR <- function(parameters_group, parameters_reference, index_mar){
    mu <- parameters_group$mu
    mu[!index_mar] <- parameters_reference$mu[!index_mar]
    pars <- list(
        mu = mu,
        sigma = parameters_group$sigma  # TODO
    )
    return(pars)
}

strategy_CR <- function(parameters_group, parameters_reference, index_mar){
    # TODO
}

strategy_CIR <- function(parameters_group, parameters_reference, index_mar){
    # TODO
}

strategy_LMCF <- function(parameters_group, parameters_reference, index_mar){
    # TODO
}






