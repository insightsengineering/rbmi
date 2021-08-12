suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
    library(glmmTMB)
})

test_that("transformation from long to wide format works", {
    vec_long = c(c(NA, 1, 2), c(1, NA, 2), c(3, NA, 4))
    vec_wide_true = rbind(c(NA, 1, 2), c(1, NA, 2), c(3, NA, 4))

    vec_wide = long2wide(vec_long = vec_long,
                         J = 3)

    vec_wide_oneElem = long2wide(vec_long = vec_long[1:3],
                                 J = 3)

    expect_true(is.matrix(vec_wide))
    expect_equal(vec_wide, vec_wide_true)

    expect_true(is.matrix(vec_wide_oneElem))
    expect_equal(vec_wide_oneElem, vec_wide_true[1,, drop = FALSE])
})

test_that("Proper missingness patterns are detected", {
    outcome = rbind(c(NA, 1, 2), c(1, NA, 2), c(3, NA, 4))

    res = get_obs_missingness_patterns(outcome)
    missingness_patterns = res$missingness_patterns
    M = res$M

    number_missingness_patterns = nrow(missingness_patterns)

    expect_equal(number_missingness_patterns, 2)
    expect_equal(missingness_patterns, rbind(c(0,1,1),c(1,0,1)))
    expect_equal(M, c(1,2,2))
})

test_that("Proper missingness pattern is detected when only one patient", {
    outcome = t(matrix(c(NA, 1, 2)))

    res = get_obs_missingness_patterns(outcome)
    missingness_patterns = res$missingness_patterns
    M = res$M

    number_missingness_patterns = nrow(missingness_patterns)

    expect_equal(number_missingness_patterns, 1)
    expect_equal(missingness_patterns, t(matrix(c(0,1,1))))
    expect_equal(M, 1)
})

test_that("Covariance matrices are sort correctly", {

    sigma_reml <- list(
        diag(rep(1,2)),
        diag(rep(1,2)) + 0.5,
        diag(rep(1,2)) + 0.8
    )

    group_sigma <- c("A", "C", "B")

    expected_output <- list(
        diag(rep(1,2)),
        diag(rep(1,2)) + 0.8,
        diag(rep(1,2)) + 0.5
    )

    ordered_sigma <- match_groups_sigmas(sigma_reml, group_sigma)

    expect_equal(ordered_sigma,
                 expected_output)

})

test_that("QR decomposition is performed correctly", {

    N <- 6
    J = 2
    designmat <- cbind(rep(1,N*J), rnorm(N*J))
    QR <- QR_decomp(designmat, N, J)

    expect_equal(QR$Q %*% QR$R, designmat)
})

test_that("List of matrices is correctly transformed into array", {

    mat <- rbind(c(1, 0.2), c(0.2, 1))
    input <- list(mat, mat+1, mat+2)

    actual_output <- listmat_to_array(input)

    expected_output <- array(NA, dim = c(3,2,2))
    for(i in 1:3) {
        expected_output[i,,] <- mat + i-1
    }
    expect_equal(actual_output, expected_output)

})

test_that("split_dim creates a list from an array as expected",
          {
              mat <- rbind(c(1, 0.2), c(0.2, 1))
              a <- array(data = NA, dim = c(3,2,2))
              for(i in 1:dim(a)[1]) {
                  a[i,,] <- mat + i-1
              }

              actual_res <- split_dim(a, 1)
              expected_res <- list(mat, mat + 1, mat + 2)
              expect_equal(actual_res, expected_res)
          }
)




set.seed(101)

n <- 500
nv <- 2

covars <- tibble(
    subjid = 1:n,
    age = rnorm(n),
    group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
    sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F")),
    strata = c(rep("A",n/2), rep("B", n/2))
)

dat <- tibble(
    subjid = rep.int(1:n, nv)
) %>%
    left_join(covars, by = "subjid") %>%
    mutate( outcome = rnorm(
        n(),
        age * 3 + (as.numeric(sex) - 1) * 3 + (as.numeric(group) - 1) * 4,
        sd = 2
    )) %>%
    arrange(subjid) %>%
    group_by(subjid) %>%
    mutate( visit = factor(paste0("Visit", 1:n())))  %>%
    ungroup() %>%
    mutate(subjid = as.character(subjid))

formula <- outcome ~ sex + group*visit
designmat <- model.matrix(formula, data = dat)

dat[sample(1:(nv*n), size = 7), "outcome"] <- NA

fit_glm <- glmmTMB::glmmTMB(
    outcome ~ sex + group*visit + (0 + visit | subjid),
    data = dat,
    dispformula = ~0,
    control = glmmTMBControl(
        optimizer = optim,
        optArgs = list(method = "BFGS"),
        parallel = 1
    ),
    REML = TRUE
)

pars <- extract_params(fit_glm)

sigma_reml <- list(
    "A" = pars$sigma$subjid,
    "B" = pars$sigma$subjid
)
betas_reml <- pars$beta

test_that("Posterior mean of mcmc equals (restricted) ML estimates", {

    set.seed(101)
    fit <- run_mcmc(
        designmat = designmat,
        outcome = dat$outcome,
        group = dat$group,
        sigma_reml = sigma_reml,
        n_imputations = 1000,
        burn_in = 200,
        burn_between = 1,
        initial_values = list(
            "beta" = betas_reml,
            "Sigma" = sigma_reml
        ),
        same_cov = TRUE,
        verbose = FALSE
    )

    s <- rstan::summary(fit$fit, pars = c("beta", "Sigma"))$summary
    post_means <- s[, "mean"]
    CI_mean_high = post_means + qnorm(0.99)*s[,"se_mean"]
    CI_mean_low = post_means - qnorm(0.99)*s[,"se_mean"]

    reml_est <- c(as.numeric(betas_reml), as.numeric(sigma_reml[[1]]))

    expect_true(is.list(fit))
    expect_true(is.list(fit$samples))
    expect_length(fit$samples$beta, 1000)

    expect_true(all(sapply(fit$samples$sigma, is.list)))
    expect_length(fit$samples$sigma, 1000)
    expect_true(all(sapply(fit$samples$sigma, function(x) length(x) == 2)))

    expect_true(all(CI_mean_low < reml_est & CI_mean_high > reml_est))
})

test_that("Warnings management works properly", {

    expect_warning( {
        set.seed(101)
        fit <- run_mcmc(
            designmat = designmat,
            outcome = dat$outcome,
            group = dat$group,
            sigma_reml = sigma_reml,
            n_imputations = 10,
            burn_in = 200,
            burn_between = 1,
            initial_values = list(
                "beta" = betas_reml,
                "Sigma" = sigma_reml
            ),
            same_cov = TRUE,
            verbose = FALSE
        )
    }
    )
})
