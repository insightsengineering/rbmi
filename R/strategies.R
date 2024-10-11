


#' Compute covariance matrix for some reference-based methods (JR, CIR)
#'
#' @description
#' Adapt covariance matrix in reference-based methods. Used for Copy Increments in
#' Reference (CIR) and Jump To Reference (JTR) methods, to adapt the covariance matrix
#' to different pre-deviation and post deviation covariance structures. See Carpenter
#' et al. (2013)
#'
#' @param sigma_group the covariance matrix with dimensions equal to `index_mar` for
#' the subjects original group
#' @param sigma_ref the covariance matrix with dimensions equal to `index_mar` for
#' the subjects reference group
#' @param index_mar A logical vector indicating which visits meet the MAR assumption
#' for the subject. I.e. this identifies the observations that after a non-MAR
#' intercurrent event (ICE).
#'
#' @references
#' Carpenter, James R., James H. Roger, and Michael G. Kenward. "Analysis of longitudinal
#' trials with protocol deviation: a framework for relevant, accessible assumptions, and
#' inference via multiple imputation." Journal of Biopharmaceutical statistics 23.6 (2013):
#' 1352-1371.
compute_sigma <- function(sigma_group, sigma_ref, index_mar) {

    if (identical(sigma_group, sigma_ref)) {
        return(sigma_ref)
    }

    size <- nrow(sigma_group)

    # i.e. if MAR assumption holds throughout the study
    if (sum(index_mar) == size) {
        return(sigma_group)
    } else if (sum(index_mar) == 0) {
        # i.e. if MAR assumption does not hold since the beginning of the study
        return(sigma_ref)
    }

    T_11 <- sigma_group[index_mar, index_mar]
    inv_R_11 <- solve(sigma_ref[index_mar, index_mar])

    sigma_11 <- T_11

    sigma_21 <- sigma_ref[!index_mar, index_mar] %*% inv_R_11 %*% T_11

    sigma_12 <- t(sigma_21)

    sigma_22 <- sigma_ref[!index_mar, !index_mar] -
        sigma_ref[!index_mar, index_mar] %*%
        inv_R_11 %*%
        (sigma_ref[index_mar, index_mar] - T_11) %*%
        inv_R_11 %*%
        sigma_ref[index_mar, !index_mar]

    sigma <- rbind(
        cbind(sigma_11, sigma_12),
        cbind(sigma_21, sigma_22)
    )

    return(sigma)
}



#' Strategies
#'
#' These functions are used to implement various reference based imputation
#' strategies by combining a subjects own distribution with that of
#' a reference distribution based upon which of their visits failed to meet
#' the Missing-at-Random (MAR) assumption.
#'
#' @param pars_group A list of parameters for the subject's group. See details.
#'
#' @param pars_ref A list of parameters for the subject's reference group. See details.
#'
#' @param index_mar A logical vector indicating which visits meet the MAR assumption
#' for the subject. I.e. this identifies the observations after a non-MAR
#' intercurrent event (ICE).
#'
#' @details
#'
#' `pars_group` and `pars_ref` both must be a list containing elements `mu` and `sigma`.
#' `mu` must be a numeric vector and `sigma` must be a square matrix symmetric covariance
#' matrix with dimensions equal to the length of `mu` and `index_mar`. e.g.
#' ```
#' list(
#'     mu = c(1,2,3),
#'     sigma = matrix(c(4,3,2,3,5,4,2,4,6), nrow = 3, ncol = 3)
#' )
#' ```
#'
#' Users can define their own strategy functions and include them via the `strategies`
#' argument to [impute()] using [getStrategies()]. That being said the following
#' strategies are available "out the box":
#'
#' - Missing at Random (MAR)
#' - Jump to Reference (JR)
#' - Copy Reference (CR)
#' - Copy Increments in Reference (CIR)
#' - Last Mean Carried Forward (LMCF)
#'
#' @name strategies
#' @export
strategy_MAR <- function(pars_group, pars_ref, index_mar) {
    return(pars_group)
}



#' @rdname strategies
#' @export
strategy_JR <- function(pars_group, pars_ref, index_mar) {

    if (all(index_mar)) {
        return(pars_group)
    } else if (all(!index_mar)) {
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


#' @rdname strategies
#' @export
strategy_CR <- function(pars_group, pars_ref, index_mar) {
    return(pars_ref)
}



#' @rdname strategies
#' @export
strategy_CIR <- function(pars_group, pars_ref, index_mar) {

    if (all(index_mar)) {
        return(pars_group)
    } else if (all(!index_mar)) {
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


#' @rdname strategies
#' @export
strategy_LMCF <- function(pars_group, pars_ref, index_mar) {
    if (all(index_mar)) {
        return(pars_group)
    } else if (all(!index_mar)) {
        stop("LMCF cannot be adopted since all outcome values are missing")
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



#' Get imputation strategies
#'
#' Returns a list defining the imputation strategies to be used to create the
#' multivariate normal distribution parameters by merging those of the source
#' group and reference group per patient.
#'
#' By default Jump to Reference (JR), Copy Reference (CR), Copy Increments in
#' Reference (CIR), Last Mean Carried Forward (LMCF) and Missing at Random (MAR)
#' are defined.
#'
#' The user can define their own strategy functions (or overwrite the pre-defined ones)
#' by specifying a named input to the function i.e. `NEW = function(...) ...`.
#' Only exception is MAR which cannot be overwritten.
#'
#' All user defined functions must take 3 inputs: `pars_group`, `pars_ref` and
#'  `index_mar`. `pars_group` and `pars_ref` are both lists with elements `mu`
#' and `sigma` representing the multivariate normal distribution parameters for
#' the subject's current group and reference group respectively.  `index_mar` will be
#' a logical vector specifying which visits the subject met the MAR assumption
#' at. The function must return a list with elements `mu` and `sigma`. See the implementation
#' of `strategy_JR()` for an example.
#'
#' @param ... User defined methods to be added to the return list. Input must
#' be a function.
#'
#' @examples
#' \dontrun{
#' getStrategies()
#' getStrategies(
#'     NEW = function(pars_group, pars_ref, index_mar) code ,
#'     JR = function(pars_group, pars_ref, index_mar)  more_code
#' )
#' }
#'
#' @export
getStrategies <- function(...) {
    user_strats <- list(...)
    pkg_strats <- list(
        "JR" = strategy_JR,
        "CR" = strategy_CR,
        "CIR" = strategy_CIR,
        "LMCF" = strategy_LMCF
    )
    for (i in names(user_strats)) {
        assert_that(
            is.function(user_strats[[i]]),
            msg = sprintf("Input %s must be a function", i)
        )
        pkg_strats[[i]] <- user_strats[[i]]
    }
    pkg_strats[["MAR"]] <- strategy_MAR
    return(pkg_strats)
}
