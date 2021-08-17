suppressPackageStartupMessages({
    library(glmmTMB)
    library(dplyr)
})

set.seed(101)

sigma <- as_covmat(c(5, 3, 8), c(0.4, 0.6, 0.3))

dat <- get_sim_data(n = 30, sigma) %>%
    mutate(outcome = if_else(rbinom(n(), 1, 0.2) == 1, NA_real_, outcome))

vars <- list(
    outcome = "outcome",
    visit = "visit",
    subjid = "subjid",
    group = "group",
    covariates = c("sex", "age", "group*visit"),
    method = "method"
)

formula <- outcome ~ sex + age + visit*group
designmat <- as_model_df(dat = dat, formula)

args_default <- list(
    designmat = designmat[, -1, drop = FALSE],
    outcome = dat$outcome,
    subjid = dat$id,
    visit = dat$visit,
    group = dat$group,
    REML = TRUE,
    cov_struct = "us",
    same_cov = TRUE,
    initial_values = NULL,
    optimizer = "L-BFGS-B"
)

# TODO
#    mmrm_df
#    mmrm_frm



test_that("MMRM model fit has expected output structure (same_cov = TRUE)",{

    ############# US
    args <- args_default
    args$cov_struct <- "us"
    fit <- do.call(fit_mmrm, args = args)
    expect_valid_fit_object(fit, "us", 3, TRUE)


    ############# TOEP
    args <- args_default
    args$cov_struct <- "toep"
    fit <- do.call(fit_mmrm, args = args)
    expect_valid_fit_object(fit, "toep", 3, TRUE)


    ############# CS
    args <- args_default
    args$cov_struct <- "cs"
    fit <- do.call(fit_mmrm, args = args)
    expect_valid_fit_object(fit, "cs", 3, TRUE)


    ############# AR1
    args <- args_default
    args$cov_struct <- "ar1"
    fit <- do.call(fit_mmrm, args = args)
    expect_valid_fit_object(fit, "ar1", 3, TRUE)
})


test_that("MMRM model fit has expected output structure (same_cov = FALSE)",{
    args <- args_default
    args$same_cov <- FALSE
    fit <- do.call(fit_mmrm, args = args)
    expect_valid_fit_object(fit, "us", 3, FALSE)
})


test_that("MMRM model fit has expected output structure (REML = FALSE)", {
    args <- args_default
    args$REML <- FALSE
    fit <- do.call(fit_mmrm, args = args)
    expect_valid_fit_object(fit, "us", 3, TRUE)
})





test_that("MMRM returns expected estimates (same_cov = TRUE)", {
    args <- args_default
    fit <- do.call(fit_mmrm, args = args)

    mod <- glmmTMB(
        outcome ~ sex + age + visit * group + us(0 + visit | id),
        dispformula = ~0,
        data = dat,
        REML = TRUE,
        control = glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "L-BFGS-B")
        )
    )
    expect_equal(fit, extract_test_fit(mod))
})


test_that("MMRM returns expected estimates (same_cov = FALSE)", {

    args <- args_default
    args$same_cov <- FALSE
    fit <- do.call(fit_mmrm, args = args)

    dat2 <- dat %>%
        mutate(GA = if_else(group == "A", 1, 0)) %>% 
        mutate(GB = if_else(group == "B", 1, 0))

    mod <- glmmTMB(
        outcome ~ sex + age + visit * group + us(0 + GA:visit | id) + us(0 + GB:visit | id),
        dispformula = ~0,
        data = dat2,
        REML = TRUE,
        control = glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "L-BFGS-B")
        )
    )

    expect_equal(fit, extract_test_fit(mod))
})



test_that("MMRM model with multiple optimizers has expected output",{

    ###### Single optimiser
    args <- args_default
    args$optimizer <- "BFGS"

    args$same_cov <- FALSE

    fit1 <- do.call(fit_mmrm_multiopt, args = args)
    fit2 <- do.call(fit_mmrm, args = args)

    expect_equal(fit1$optimizer, "BFGS")
    fit1$optimizer <- NULL

    expect_equal(fit1, fit2)
    expect_valid_fit_object(fit1, "us", 3, FALSE)


    ###### Multiple optimisers
    args$optimizer <- c("BFGS", "Nelder-Mead", "L-BFGS-B")

    fit3 <- do.call(fit_mmrm_multiopt, args = args)

    expect_equal(fit3$optimizer, "BFGS")
    fit3$optimizer <- NULL

    expect_valid_fit_object(fit3, "us", 3, FALSE)
    expect_equal(fit3, fit2)
})





test_that("MMRM returns expected estimates under different model specifications", {

    set.seed(101)

    sigma <- as_covmat(c(5, 3, 8), c(0.4, 0.6, 0.3))

    dat <- ife(
        is_nightly(),
        get_sim_data(n = 300, sigma),
        get_sim_data(n = 50, sigma)
    )

    dat <- dat %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.2) == 1, NA_real_, outcome))


    runtests <- function(same_cov, scale) {

        formula_expr <- "outcome ~ sex*visit + age*visit + visit*group"
        test_mmrm_numeric(dat, formula_expr, same_cov, scale)

        formula_expr <- "outcome ~ age:sex^2 + sex:age*group + visit*group"
        test_mmrm_numeric(dat, formula_expr, same_cov, scale)

        if (is_nightly()) {
            formula_expr <- "outcome ~ sex*group + age*group + visit*group"
            test_mmrm_numeric(dat, formula_expr, same_cov, scale)

            formula_expr <- "outcome ~ sex*group*visit + age*group*visit + visit*group"
            test_mmrm_numeric(dat, formula_expr, same_cov, scale)

            formula_expr <- "outcome ~ sex + age + sex:age + sex*visit + age:group + visit*group"
            test_mmrm_numeric(dat, formula_expr, same_cov, scale)

            formula_expr <- "outcome ~ visit + age*visit*group + sex + visit*group"
            test_mmrm_numeric(dat, formula_expr, same_cov, scale)

            formula_expr <- "outcome ~ sex^2"
            test_mmrm_numeric(dat, formula_expr, same_cov, scale)
        }

    }

    runtests(TRUE, FALSE)
    runtests(TRUE, TRUE)

    if (is_nightly()) {
        runtests(FALSE, FALSE)
        runtests(FALSE, TRUE)
    }

})