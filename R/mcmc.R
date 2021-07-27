long2wide <- function(vec_long,
                      J) {
    len <- length(vec_long)

    assert_that(
        len%%J == 0,
        msg = "length(vec_long) must be a multiple of J"
    )

    wide_mat <- matrix(vec_long, nrow = len/J, ncol = J, byrow = TRUE)
    return(wide_mat)
}

get_obs_missingness_patterns <- function(outcome) {

    assert_that(
        is.matrix(outcome),
        msg = "outcome must be a matrix"
    )

    y_obs <- matrix(1, nrow = nrow(outcome), ncol = ncol(outcome))
    y_obs[is.na(outcome)] <- 0
    missingness_patterns <- unique(y_obs)
    M <- apply(y_obs,
               1,
               function(y) which(apply(missingness_patterns, 1, function(x) identical(x, y))==TRUE))

    return(list(missingness_patterns = missingness_patterns,
                M = M))
}

match_groups_sigmas <- function(
    sigma_reml,
    levels_group
) {

    return(sigma_reml[order(levels_group)])

}

QR_decomp <- function(designmat, N, J) {
    qr_obj = qr(designmat)
    Q = qr.Q(qr = qr_obj)* sqrt(N*J - 1)
    R = qr.R(qr = qr_obj)/ sqrt(N*J - 1)

    ret_obj <- list(
        Q = Q,
        R = R
    )

    return(ret_obj)
}

run_mcmc <- function(
    designmat,
    outcome,
    group,
    sigma_reml,
    n_imputations,
    burn_in,
    burn_between,
    initial_values,
    same_cov
) {
    data <- prepare_data_mcmc(
        designmat,
        outcome,
        group,
        same_cov,
        sigma_reml
    )

    fit <- fit_mcmc(
        data,
        n_imputations,
        burn_in,
        burn_between,
        initial_values,
        same_cov
    )

    check_mcmc(fit, n_imputations)

    draws <- extract_draws(fit)

    ret_obj <- list(
        "samples" = draws,
        "fit" = fit
    )

    return(ret_obj)
}

prepare_data_mcmc <- function(
    designmat,
    outcome,
    group,
    same_cov,
    sigma_reml) {

    assert_that(
        is.list(sigma_reml),
        msg = "sigma_reml must be a list of covariance matrices"
    )

    assert_that(
        is.factor(group),
        msg = "group must be a factor"
    )

    J <- nrow(sigma_reml[[1]])

    outcome <- long2wide(vec_long = outcome,
                         J = J)
    N <- nrow(outcome)

    obs_miss_patterns <- get_obs_missingness_patterns(outcome)
    outcome[is.na(outcome)] <- 1000

    QR_mat <- QR_decomp(designmat, N, J)

    if(same_cov) {
        data <- list(J = J,
                     N = N,
                     P = ncol(designmat),
                     n_missingness_patterns = nrow(obs_miss_patterns$missingness_patterns),
                     M = obs_miss_patterns$M,
                     Q = QR_mat$Q,
                     R = QR_mat$R,
                     y = outcome,
                     y_observed = obs_miss_patterns$missingness_patterns,
                     Sigma_reml = sigma_reml[[1]])
    } else {

        assert_that(
            length(sigma_reml) == nlevels(group),
            msg = "The number of covariance matrices must be equal to the number of groups"
        )

        G <- nlevels(group)
        which_arm <- as.numeric(group)[seq(1, N*J, by = J)]

        sigma_reml <- match_groups_sigmas(sigma_reml, levels(group))

        data <- list(J = J,
                     N = N,
                     P = ncol(designmat),
                     n_missingness_patterns = nrow(obs_miss_patterns$missingness_patterns),
                     M = obs_miss_patterns$M,
                     Q = QR_mat$Q,
                     R = QR_mat$R,
                     y = outcome,
                     y_observed = obs_miss_patterns$missingness_patterns,
                     Sigma_reml = sigma_reml,
                     G = G,
                     which_arm = which_arm)
    }

}

#' @import Rcpp
#' @import methods
#' @useDynLib rbmi, .registration = TRUE
#' @importFrom rstan sampling
fit_mcmc <- function(
    data,
    n_imputations,
    burn_in,
    burn_between,
    initial_values,
    same_cov) {

    initial_values = list(initial_values)

    if(same_cov) {

        suppressWarnings({
            stan_fit <- sampling(
                object = stanmodels$MMRM_same_cov,
                data = data,
                pars = c("beta", "Sigma"),
                chains = 1,
                warmup = burn_in,
                thin = burn_between,
                iter = burn_in + burn_between*n_imputations,
                init = initial_values)
        })

    } else {

        suppressWarnings({
            stan_fit <- sampling(
                object = stanmodels$MMRM_diff_cov,
                data = data,
                pars = c("beta", "Sigma"),
                chains = 1,
                warmup = burn_in,
                thin = burn_between,
                iter = burn_in + burn_between*n_imputations,
                init = initial_values)
        })

    }

    return(stan_fit)

}

#' Title
split_dim <- function(a, n) {
    setNames(lapply(split(a, arrayInd(seq_along(a), dim(a))[, n]),
                    array, dim = dim(a)[-n], dimnames(a)[-n]),
             dimnames(a)[[n]])

}

#' @importFrom rstan extract
extract_draws <- function(stan_fit) {

    pars <- extract(stan_fit, pars = c("beta", "Sigma"))
    names(pars) <- c("beta", "sigma")

    ##################### from array to list
    pars$sigma <- split_dim(pars$sigma, 1)

    if(length(dim(pars$sigma[[1]])) == 3) { # if same_cov == FALSE
        pars$sigma <- lapply(
            pars$sigma,
            function(x) split_dim(x, 1)
        )
    } else {
        pars$sigma <- lapply(
            pars$sigma,
            function(x) list(x,x)
        )
    }

    pars$beta <- split_dim(pars$beta, 1)
    pars$beta <- lapply(pars$beta, as.vector)

    return(pars)
}

#' @importFrom rstan summary
get_ESS <- function(stan_fit) {

    return(rstan::summary(stan_fit, pars = c("beta", "Sigma"))$summary[,"n_eff"])
}

check_ESS <- function(stan_fit, n_draws, threshold = 0.4) {

    ESS <- get_ESS(stan_fit)

    n_low_ESS <- sum(ESS/n_draws < threshold)

    if(any(ESS/n_draws < threshold)) {
        warning(paste0("The Effective Sample Size is below ",threshold*100,"% for ", n_low_ESS ," parameters. Please consider increasing burn-in and/or burn-between"),
                call. = FALSE)
    }

    return(invisible(NULL))
}

#' @importFrom rstan get_divergent_iterations get_bfmi get_max_treedepth_iterations
check_hmc_diagn <- function(stan_fit) {

    if(any(get_divergent_iterations(stan_fit)) # draws "out of the distribution"
       | isTRUE(get_bfmi(stan_fit) < 0.2 ) # exploring well the target distribution
       | any(get_max_treedepth_iterations(stan_fit))) # efficiency of the algorithm
    {
        warning("Lack of efficiency in the HMC sampler: please consider increasing the burn-in period.",
                call. = FALSE)
    }

    return(invisible(NULL))
}

check_mcmc <- function(stan_fit, n_draws, threshold_lowESS = 0.4) {

    check_ESS(
        stan_fit = stan_fit,
        n_draws = n_draws,
        threshold = threshold_lowESS
    )

    check_hmc_diagn(stan_fit)

    return(invisible(NULL))
}
