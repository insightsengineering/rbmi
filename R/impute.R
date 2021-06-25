

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

    samples_grouped <- group_samples_by_id(draws$samples)

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


#' Transpose Samples
#'
#'@param samples TODO
#' This function converts a list of Beta/Sigma samples into a more
#' efficient object.
#'
#' Betas will be stored as a matrix with 1 row per sample and
#' 1 column per parameter
#'
#' Sigma will be stored as a k array of i by i matrices where i is the
#' number of visits and k is the number of samples i.e. dim = c(i,i,k)
#'
#' Indexes will be a named list of vectors where the name corresponds
#' to the subject ID and the vector represents which rows from beta and
#' elements from sigma are used by that subject. (i.e. which samples did
#' the subject get used in) If a patient appears in a sample more than once
#' then that number will be duplicated in the index vector i.e. c(1,2,2,2,3,6)
group_samples_by_id <- function(samples){

    beta <- list()
    index <- list()
    sigma <- list()

    grp_names <- names(samples[[1]]$sigma)

    n_parameters <- length(samples[[1]]$beta)
    n_samples <- length(samples)
    n_visits <- nrow(samples[[1]]$sigma[[1]])

    for( grp in grp_names) {
        sigma[[grp]] <-  array(dim = c(n_visits, n_visits, n_samples))
    }

    beta <- matrix(ncol = n_parameters, nrow = n_samples)

    for( i in seq_along(samples)){
        sample <- samples[[i]]
        beta[i,] <- sample$beta
        for( grp in grp_names) sigma[[grp]][,,i] <-  sample$sigma[[grp]]
        for( j in sample$ids) index[[j]] <- c(index[[j]], i)
    }

    list(
        beta = beta,
        sigma = sigma,
        index = index
    )
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

    parameters_group <- list(
        mu = get_mu(dat_pt_mod[,-1], t(beta[index,])),
        sigma = sigma[[group_pt]][,,index]
    )

    parameters_reference <- list(
        mu = get_mu(dat_ref_mod[,-1], t(beta[index,])),
        sigma = sigma[[group_ref]][,,index]
    )

    pars <- pars_apply(
        pars1 = parameters_group,
        pars2 = parameters_reference,
        fun = strategies[[strategy]],
        index_mar = index_mar
    )

    conditional_parameters <- pars_apply(
        pars1 = pars,
        fun = get_conditional_parameters,
        values = values
    )

    imputed_outcome <- impute_outcome(conditional_parameters)
    result$values <- imputed_outcome
    return(result)
}


impute_outcome <- function(conditional_parameters){
    ndim <- dim(conditional_parameters$sigma)[3]
    outcome <- matrix(
        nrow = nrow(conditional_parameters$mu),
        ncol = ncol(conditional_parameters$mu),
    )
    for(i in 1:ndim){
        outcome[,i] <- mvtnorm::rmvnorm(
            n = 1,
            mean = conditional_parameters$mu[,i],
            sigma = matrix(
                conditional_parameters$sigma[,,i, drop = FALSE],
                nrow = nrow(conditional_parameters$mu)
            )
        )
    }
    return(outcome)

}


#' Apply function across pars objects
#'
#' @param fun TODO
#' @param pars1 TODO
#' @param pars2 TODO
#' @param ... TODO
#'
#' The purpose of this function is to apply another function
#' across a parameters object (a list with elements mu and sigma
#' which are a matrix and an array respectively) in a way
#' such that the function being applied can interact with the inputs as
#' a mean vector and a single covariance matrix respectively.
#'
#' That is to say, this function will loop over pars running:
#' fun(
#'     list( pars1$mu[,i], pars1$sigma[,,i]),
#'     list( pars2$mu[,i], pars2$sigma[,,i]),
#'     ...
#' )
#'
#' The return type from fun() should be a list with elements mu as a vector
#' and sigma as a matrix.
pars_apply <- function(
    fun,
    pars1,
    pars2 = NULL,
    ...
){
    stopifnot(
        dim(dim(pars1$sigma))[3] == ncol(pars1$mu),
        is.function(fun)
    )
    if(!is.null(pars2)){
        stopifnot(
            all(dim(pars1$mu) == dim(pars2$mu)),
            all(dim(pars1$sigma) == dim(pars2$sigma))
        )
    }

    result <- list()
    for( i in 1:ncol(pars1$mu)){
        args <- list()
        args[[1]] <- list(
            mu = pars1$mu[,i],
            sigma = pars1$sigma[,,i]
        )
        if(!is.null(pars2)){
            args[[2]] <- list(
                mu = pars2$mu[,i],
                sigma = pars2$sigma[,,i]
            )
        }
        args <- append(args, list(...))
        result[[i]] <- do.call(fun, args)
    }
    mu_list <- lapply(result, function(x) x[["mu"]])
    sigma_list <- lapply(result, function(x) x[["sigma"]])

    new_mu <- matrix(
        ncol = length(mu_list),
        nrow = length(mu_list[[1]])
    )
    for( i in seq_along(mu_list)){
        new_mu[,i] <- mu_list[[i]]
    }

    new_sigma <- array(
        dim = c(
            nrow(new_mu),
            nrow(new_mu),
            length(sigma_list)
        )
    )
    for(i in seq_along(sigma_list)){
        new_sigma[,,i] <- sigma_list[[i]]
    }

    pars <- list(
        mu = new_mu,
        sigma = new_sigma
    )
    return(pars)
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
