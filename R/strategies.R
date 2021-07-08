#' Compute covariance matrix for some reference-based methods (JR, CIR)
#'
#' @description
#' Adapt covariance matrix in reference-based methods. Used for Copy Increments in
#' Reference (CIR) and Jump To Reference (JTR) methods, to adapt the covariance matrix
#' to different pre-deviation and post deviation covariance structures.
#'
#' TODO (params)
#'
#' @references
#' TODO    insertRef{carpenter2013analysis}{rbmi}
compute_sigma <- function(sigma_group, sigma_ref, index_mar){

    if(identical(sigma_group, sigma_ref)) {
        return(sigma_ref)
    }

    size <- nrow(sigma_group)

    # i.e. if MAR assumption holds throughout the study
    if(sum(index_mar) == size) {
        return(sigma_group)
    } else if(sum(index_mar) == 0) {
        # i.e. if MAR assumption does not hold since the beginning of the study
        return(sigma_ref)
    }

    first_nonMAR <- which(!index_mar)[1]
    last_MAR <- first_nonMAR -1

    T_11 <- sigma_group[1:last_MAR, 1:last_MAR]
    inv_R_11 <- solve(sigma_ref[1:last_MAR, 1:last_MAR])

    sigma_11 <- T_11

    sigma_21 <- sigma_ref[first_nonMAR:size, 1:last_MAR]%*%inv_R_11%*%T_11

    sigma_12 <- t(sigma_21)

    sigma_22 <- sigma_ref[first_nonMAR:size,first_nonMAR:size] -
        sigma_ref[first_nonMAR:size, 1:last_MAR] %*%
        inv_R_11 %*%
        (sigma_ref[1:last_MAR, 1:last_MAR]-T_11) %*%
        inv_R_11 %*%
        sigma_ref[1:last_MAR, first_nonMAR:size]

    sigma <- rbind(
        cbind(sigma_11,sigma_12),
        cbind(sigma_21,sigma_22)
    )

    return(sigma)
}


strategy_MAR <- function(pars_group, pars_ref, index_mar){
    return(pars_group)
}


strategy_JR <- function(pars_group, pars_ref, index_mar){

    if(sum(index_mar) == length(pars_group$mu)) {
        return(pars_group)
    } else if(sum(index_mar) == 0) {
        return(pars_ref)
    }

    mu <- pars_group$mu
    mu[!index_mar] <- pars_ref$mu[!index_mar]

    sigma <- compute_sigma(
        sigma_group = pars_group$sigma,
        sigma_ref = pars_ref$sigma,
        index_mar = index_mar
    )

    pars <- list(
        mu = mu,
        sigma = sigma
    )

    return(pars)
}


strategy_CR <- function(pars_group, pars_ref, index_mar){
    return(pars_ref)
}


strategy_CIR <- function(pars_group, pars_ref, index_mar){

    if(sum(index_mar) == length(pars_group$mu)) {
        return(pars_group)
    } else if(sum(index_mar) == 0) {
        return(pars_ref)
    }

    mu <- pars_group$mu
    last_mar <- which(!index_mar)[1] - 1
    increments_from_last_mar_ref <- pars_ref$mu[!index_mar] - pars_ref$mu[last_mar]
    mu[!index_mar] <- mu[last_mar] + increments_from_last_mar_ref

    sigma <- compute_sigma(
        sigma_group = pars_group$sigma,
        sigma_ref = pars_ref$sigma,
        index_mar = index_mar
    )

    pars <- list(
        mu = mu,
        sigma = sigma
    )

    return(pars)
}


strategy_LMCF <- function(pars_group, pars_ref, index_mar){

    if(sum(index_mar) == length(pars_group$mu)) {
        return(pars_group)
    } else if(sum(index_mar) == 0) {
        # TODO
        return()
    }

    mu <- pars_group$mu
    last_mar <- which(!index_mar)[1] - 1
    mu[!index_mar] <- mu[last_mar]

    pars <- list(
        mu = mu,
        sigma = pars_group$sigma
    )

    return(pars)
}



#' Define imputation strategies
#'
#' Returns a list defining the imputation strategies to be used to create the
#' multivariate normal distribution parameters by merging those of the source group and reference
#' group per patient.
#'
#' By default Jump to Reference (JR), Copy Reference (CR),
#' Copy Increments from Reference (CIR), Last Mean Carried Forward (LMCF) and
#' Missing at Random (MAR) are defined.
#'
#' The user can define their own strategy functions (or overwrite the pre-defined ones)
#' by specifying a named input to the function i.e. `NEW = function(...) ...`.
#' Only exception is MAR which cannot be overwritten
#'
#' All user defined functions must take 3 inputs `pars_group`, `pars_ref` & `index_mar`.
#' `pars_group` and `pars_ref` are both lists with elements `mu` and `sigma` representing
#' the multivariate normal distribution parameters for the subjects current and reference group
#' respectively.  `index_mar` will be a logical vector specifying which visits the subject met
#' the MAR assumption at. The function must return a list with elements `mu` and `sigma`.
#'
#' @param ... User defined methods to be added to the return list. Input must be a function
#'
#' @examples
#' \dontrun{
#' getStrategies()
#' getStrategies(
#'     NEW = function(pars_group, pars_ref, index_mar) <code>,
#'     JR = function(pars_group, pars_ref, index_mar) <more code>
#' )
#' }
#'
#' @export
getStrategies <- function(...){
    user_strats <- list(...)
    pkg_strats <- list(
        "JR" = strategy_JR,
        "CR" = strategy_CR,
        "CIR" = strategy_CIR,
        "LMCF" = strategy_LMCF
    )
    for(i in names(user_strats)) {
        assert_that(
            is.function(user_strats[[i]]),
            msg = sprintf("Input %s must be a function", i)
        )
        pkg_strats[[i]] <- user_strats[[i]]
    }
    pkg_strats[["MAR"]] <- strategy_MAR
    return(pkg_strats)
}
