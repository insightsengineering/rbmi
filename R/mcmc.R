long2wide <- function(vec_long,
                      J) {
  len <- length(vec_long)
  stopifnot(len%%J == 0)
  wide_mat <- matrix(vec_long, nrow = len/J, ncol = J, byrow = TRUE)
  return(wide_mat)
}

get_obs_missingness_patterns <- function(outcome) {
  
  stopifnot(is.matrix(outcome))
  
  y_obs <- matrix(1, nrow = nrow(outcome), ncol = ncol(outcome))
  y_obs[is.na(outcome)] <- 0
  missingness_patterns <- unique(y_obs)
  M <- apply(y_obs,
             1,
             function(y) which(apply(missingness_patterns, 1, function(x) identical(x, y))==TRUE))
  
  return(list(missingness_patterns = missingness_patterns,
              M = M))
}

prepare_data_mcmc <- function(outcome,
                              groups,
                              Q,
                              R,
                              same_cov,
                              Sigma_reml) {
  
  stopifnot(
    is.list(Sigma_reml) |
      is.factor(groups)
  )
  
  J <- nrow(Sigma_reml[[1]])
  
  outcome <- long2wide(vec_long = outcome,
                       J = J)
  N <- nrow(outcome)
  
  obs_miss_patterns <- get_obs_missingness_patterns(outcome)
  outcome[is.na(outcome)] <- 1000
  
  if(same_cov) {
    data <- list(J = J,
                 N = N,
                 P = ncol(Q),
                 n_missingness_patterns = nrow(obs_miss_patterns$missingness_patterns),
                 M = obs_miss_patterns$M,
                 Q = Q,
                 R = R,
                 y = outcome,
                 y_observed = obs_miss_patterns$missingness_patterns,
                 Sigma_reml = Sigma_reml[[1]])
  } else {
    G <- nlevels(groups)
    which_arm <- as.numeric(groups)[seq(1, N*J, by = J)]
    
    data <- list(J = J,
                 N = N,
                 P = ncol(Q),
                 n_missingness_patterns = nrow(obs_miss_patterns$missingness_patterns),
                 M = obs_miss_patterns$M,
                 Q = Q,
                 R = R,
                 y = outcome,
                 y_observed = obs_miss_patterns$missingness_patterns,
                 Sigma_reml = Sigma_reml,
                 G = G,
                 which_arm = which_arm)
  }
  
}

#' @importFrom rstan sampling
run_mcmc <- function(
  stan_model,
  data,
  n_imputations,
  burn_in,
  burn_between,
  initial_values) {
  
  suppressWarnings({
    stan_fit <- sampling(
      object = stan_model,
      data = data,
      pars = c("beta", "Sigma"),
      chains = 1,
      warmup = burn_in,
      thin = burn_between,
      iter = burn_in + burn_between*n_imputations,
      init = initial_values)
  })
  
  return(stan_fit)
  
}

#' @importFrom rstan extract
extract_draws <- function(stan_fit) {
  
  return(extract(stan_fit, pars = c("beta", "Sigma")))
}

get_draws_sigma_upper_tri <- function(draws_Sigma) {
  # select upper triangular elements
  if(length(dim(draws_Sigma)) == 4) { # if same_cov == FALSE
    G <- dim(draws_Sigma)[2]
    s_tri <- lapply(1:G,
                    function(i) apply(array(draws_Sigma[,i,,], dim = dim(draws_Sigma)[c(1,3,4)]),
                                      1,
                                      function(x) x[upper.tri(x, diag = TRUE)]))
    s_tri <- t(do.call(rbind, s_tri))
  } else {
    s_tri <- t(apply(draws_Sigma, 1, function(x) x[upper.tri(x, diag = TRUE)]))
  }
}

compute_acf <- function(vec) {
  
  return(as.numeric(acf(vec, plot = FALSE, lag.max = 1, type = "partial")[[1]]))
}

compute_acf_pars <- function(pars) {
  
  beta <- pars$beta
  Sigma <- pars$Sigma
  
  s_tri <- get_draws_sigma_upper_tri(Sigma)
  
  # autocorrelation analysis
  ac_beta <- apply(beta, 2, compute_acf)
  ac_sigma <- apply(s_tri, 2, compute_acf)
  
  return(list("ac_beta" = ac_beta,
              "ac_sigma" = ac_sigma))
}

correlation_test <- function(corr, sample_size) {
  
  t_stat <- corr*sqrt((sample_size - 2)/(1-corr^2))
  p_value <- 2*(1-pt(abs(t_stat), df = sample_size - 2))
  
  return(p_value)
}

bonferroni_correction <- function(p_value_vec, alpha = 0.05) {
  
  return(p_value_vec <= alpha/length(p_value_vec))
}

check_autocorr <- function(pars, alpha = 0.05) {
  
  ac_pars <- compute_acf_pars(pars)
  ac_betas <- ac_pars$ac_beta
  ac_sigma <- ac_pars$ac_sigma
  
  betas_pvalues <- sapply(ac_betas, function(x) correlation_test(x, sample_size = n_imputations-1))
  sigma_pvalues <- sapply(ac_sigma, function(x) correlation_test(x, sample_size = n_imputations-1))
  
  p_values <- c(betas_pvalues, sigma_pvalues)
  
  rejections <- bonferroni_correction(
    p_value_vec = p_values,
    alpha = alpha)
  
  if(any(rejections)) {
    warning("The autocorrelation between consecutive draws is significantly non zero: please consider increasing burn-in and/or burn_between",
            call. = FALSE)
  }
  
  return(invisible(NULL))
}

#' @importFrom rstan summary
get_ESS <- function(stan_fit) {

  return(rstan::summary(stan_fit, pars = c("beta", "Sigma"))$summary[,"n_eff"])
}

check_ESS <- function(stan_fit, n_draws, threshold = 0.5) {
  
  ESS <- get_ESS(stan_fit)
  
  if(any(ESS/n_draws < threshold)) {
    warning("The Effective Sample Size is low: please consider increasing burn-in and/or burn-between",
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
    warning("Lack of efficiency in the HMC sampler: please analyze better the `stanfit` object",
            call. = FALSE)
  } 
  
  return(invisible(NULL))
}

check_mcmc <- function(stan_fit, threshold_lowESS = 0.5, alpha = 0.05) {
  
  pars <- extract_draws(stan_fit)
  n_draws <- nrow(pars$beta)
  
  check_autocorr(
    pars = pars,
    alpha = alpha)
  
  check_ESS(
    stan_fit = stan_fit,
    n_draws = n_draws,
    threshold = threshold_lowESS)
  
  check_hmc_diagn(stan_fit)
  
  return(invisible(NULL))
}
