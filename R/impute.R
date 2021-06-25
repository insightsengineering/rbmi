

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
            strategies = strategies
        ),
        SIMPLIFY = FALSE
    )
    return(imputes)
}



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


invert_indexes <- function(x){
    lens <- vapply(x, function(x) length(x), numeric(1))
    grp <- rep(seq_along(x), lens)
    vals <- unlist(x, use.names = FALSE)
    index <- split(grp, vals)
    names(index) <- unique(vals)
    return(index)
}


impute_data_individual <- function(
    id,
    index,
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

    # Dummy outcome value to stop rows being dropped by model.matrix
    dat_pt[,vars$outcome] <- 1

    # TODO - Enforce group as a factor ?
    dat_ref <- dat_pt
    dat_ref[,vars$group] <- factor(group_ref, levels = levels(group_pt))

    dat_pt_mod <- as_model_df(dat_pt, as_simple_formula(vars))
    dat_ref_mod <- as_model_df(dat_ref, as_simple_formula(vars))

    parameters_group <- get_parameters(
        dat = dat_pt_mod[-1],
        beta = beta[index],
        sigma = sigma[[group_pt]][index]
    )

    parameters_reference <- get_parameters(
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

    imputed_outcome <- lapply(conditional_parameters, impute_outcome)
    result$values <- imputed_outcome
    return(result)
}


get_parameters <- function(dat, beta, sigma){
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
    return(result)
}


get_conditional_parameters <- function(pars, values){
    is_miss <- is.na(values)

    if(sum(is_miss) == length(values)){
        return(pars)
    }

    a <- values[!is_miss]
    k <- sum(is_miss)
    j <- sum(!is_miss)
    mu1 <- matrix(nrow = k, pars$mu[is_miss])
    mu2 <- matrix(nrow = j, pars$mu[!is_miss])
    sig11 <- matrix(nrow =k, ncol = k, pars$sigma[is_miss,is_miss])
    sig12 <- matrix(nrow =k, ncol = j, pars$sigma[is_miss,!is_miss])
    sig21 <- matrix(nrow =j, ncol = k, pars$sigma[!is_miss,is_miss])
    sig22 <- matrix(nrow =j, ncol = j, pars$sigma[!is_miss,!is_miss])
    sig22_inv <- solve(sig22)
    sig22_inv_12 <-  sig12 %*% sig22_inv
    list(
        mu = mu1 -  sig22_inv_12 %*% (a - mu2),
        sigma = sig11 + sig22_inv_12 %*% sig21
    )
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




strategy_MAR <- function(pars_group, pars_ref, index_mar){
    return(pars_group)
}

strategy_JR <- function(pars_group, pars_ref, index_mar){
    mu <- pars_group$mu
    mu[!index_mar] <- pars_ref$mu[!index_mar]
    pars <- list(
        mu = mu,
        sigma = pars_group$sigma  # TODO
    )
    return(pars)
}

strategy_CR <- function(pars_group, pars_ref, index_mar){
    # TODO
}

strategy_CIR <- function(pars_group, pars_ref, index_mar){
    # TODO
}

strategy_LMCF <- function(pars_group, pars_ref, index_mar){
    # TODO
}
