suppressPackageStartupMessages({
    library(glmmTMB)
    library(dplyr)
})



compute_n_params <- function(cov_struct, nv) {
    n_params <- switch(cov_struct,
        "us" = nv * (nv + 1) / 2,
        "toep" = 2 * nv - 1,
        "cs" = nv + 1,
        "ar1" = 2
    )
    return(n_params)
}



# function for checking whether x is a formula object
is.formula <- function(x) {
    is.call(x) && x[[1]] == quote(`~`)
}




expect_valid_fit_object <- function(fit, cov_struct, nv, same_cov) {

    n_params <- compute_n_params(cov_struct, nv)

    if (!same_cov) {
        n_params <- 2 * n_params
    }


    expect_type(fit, "list")
    expect_length(fit, 4)

    expect_true(all(names(fit) %in% c("beta", "sigma", "failed", "theta")))

    expect_vector(fit$beta)
    expect_length(fit$beta, 8)

    expect_type(fit$sigma, "list")
    expect_length(fit$sigma, 2)
    expect_true(is.matrix(fit$sigma[[1]]))
    expect_equal(dim(fit$sigma[[1]]), c(nv,nv))
    expect_true(is.matrix(fit$sigma[[2]]))
    expect_equal(dim(fit$sigma[[2]]), c(nv,nv))

    expect_vector(fit$theta)
    expect_length(fit$theta, n_params)

    expect_true(fit$failed %in% c(TRUE, FALSE))
}



extract_test_fit <- function(mod) {

    beta <- fixef(mod)$cond
    names(beta) <- NULL

    sigma <- VarCorr(mod)$cond
    sigma <- lapply(sigma, function(x) {
        x <- as.matrix(data.frame(x))
        rownames(x) <- NULL
        colnames(x) <- NULL
        return(x)
    })
    sigma <- ife(
        length(sigma) == 1,
        list("A" = sigma[[1]], "B" = sigma[[1]]),
        list("A" = sigma[[1]], "B" = sigma[[2]])
    )
    theta <- getME(mod, name = "theta")

    converged <- ifelse(mod$fit$convergence == 0, TRUE, FALSE)

    output_expected <- list(
        beta = beta,
        sigma = sigma,
        theta = theta,
        failed = !converged
    )
    return(output_expected)
}




test_mmrm_numeric <- function(dat, formula_expr, same_cov, scale = FALSE) {

    formula <- as.formula(formula_expr)
    designmat <- as_model_df(dat, formula)

    if(scale){
        dat_limit <- ceiling(nrow(dat) / 2)
        scaler <- scalerConstructor$new(designmat[seq_len(dat_limit),])
        dmat <- scaler$scale(designmat)
    } else {
        dmat <- designmat
    }

    fit_actual <- fit_mmrm(
        designmat = dmat[, -1, drop = FALSE],
        outcome = as.data.frame(dmat)[[1]],
        subjid = dat$id,
        visit = dat$visit,
        group = dat$group,
        cov_struct = "us",
        REML = TRUE,
        same_cov = same_cov,
        optimizer = "BFGS"
    )

    if(scale){
        fit_actual$beta <- scaler$unscale_beta(fit_actual$beta)
        fit_actual$sigma <- lapply(fit_actual$sigma, function(x) scaler$unscale_sigma(x))
    }

    covariance <- ife(
        same_cov,
        " + us(0 + visit | id)",
        " + us(0 + GA:visit | id) + us(0 + GB:visit | id)"
    )

    if(!same_cov){
        dat <- dat %>%
            mutate(GA = if_else(group == "A", 1, 0)) %>%
            mutate(GB = if_else(group == "B", 1, 0))
    }

    mod <- glmmTMB(
        as.formula(paste0(formula_expr, covariance)),
        dispformula = ~0,
        data = dat,
        REML = TRUE,
        control = glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "BFGS"),
            parallel = 1
        )
    )

    fit_expected <- extract_test_fit(mod)

    if (scale) {
        expect_true(all(
            abs(fit_actual$beta - fit_expected$beta) < 0.001
        ))

        limit_a <- fit_expected$sigma[["A"]] - (fit_expected$sigma[["A"]] * 0.99)
        limit_b <- fit_expected$sigma[["B"]] - (fit_expected$sigma[["B"]] * 0.99)

        expect_true(all(
            abs(fit_actual$sigma[["A"]] - fit_expected$sigma[["A"]]) < limit_a
        ))

        expect_true(all(
            abs(fit_actual$sigma[["B"]] - fit_expected$sigma[["B"]]) < limit_b
        ))

    } else {
        expect_equal(fit_actual, fit_expected)
    }
}




set.seed(101)

sigma <- as_covmat(c(5, 3, 8), c(0.4, 0.6, 0.3))

dat <- get_sim_data(n = 40, sigma) %>%
    mutate(outcome = if_else(rbinom(n(), 1, 0.2) == 1, NA_real_, outcome))

vars <- ivars(
    outcome = "outcome",
    visit = "visit",
    subjid = "subjid",
    group = "group",
    covariates = c("sex", "age", "group*visit"),
    strategy = "strategy"
)

formula <- outcome ~ sex + age + visit * group
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



test_that("as_mmrm_df & as_mmrm_formula", {

    sigma <- as_covmat(c(2, 6, 3), c(0.4,0.7,0.5))
    dat <- get_sim_data(100, sigma)


    #### Without Groupings
    x <- as_mmrm_df(
        designmat = dat,
        outcome = dat$outcome,
        visit = dat$visit,
        subjid = dat$id
    )

    expect_equal(ncol(x), ncol(dat) + 3)
    expect_equal(
        colnames(x),
        c(paste0("V", seq_len(ncol(dat))), "outcome", "visit", "subjid")
    )
    expect_equal(nrow(x), nrow(dat))

    frm_actual <- as_mmrm_formula(x, "us")
    frm_expected <- outcome ~ V1 + V2 + V3 + V4 + V5 + V6 + us(0 + visit | subjid) - 1
    expect_equal(frm_expected, frm_actual, ignore_attr = TRUE)


    #### With Groupings
    x <- as_mmrm_df(
        designmat = dat,
        outcome = dat$outcome,
        visit = dat$visit,
        subjid = dat$id,
        groups = sample(c("A", "B", "C"), size = nrow(x), replace = TRUE)
    )

    expect_equal(ncol(x), ncol(dat) + 6)
    expect_equal(
        colnames(x),
        c(paste0("V", seq_len(ncol(dat))), "outcome", "visit", "subjid", "G1", "G2", "G3")
    )
    expect_equal(nrow(x), nrow(dat))


    frm_actual <- as_mmrm_formula(x, "us")
    frm_expected <- outcome ~ V1 + V2 + V3 + V4 + V5 + V6 +
        us(0 + G1:visit | subjid) +
        us(0 + G2:visit | subjid) +
        us(0 + G3:visit | subjid) - 1
    expect_equal(frm_expected, frm_actual, ignore_attr = TRUE)


    frm_actual <- as_mmrm_formula(x, "toep")
    frm_expected <- outcome ~ V1 + V2 + V3 + V4 + V5 + V6 +
        toep(0 + G1:visit | subjid) +
        toep(0 + G2:visit | subjid) +
        toep(0 + G3:visit | subjid) - 1
    expect_equal(frm_expected, frm_actual, ignore_attr = TRUE)
    expect_error(as_mmrm_formula(x, "toep2"), regexp = "'arg' should be one of")
})




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



test_that("MMRM model with multiple optimizers has expected output", {

    ###### Single optimiser
    args <- args_default
    args$optimizer <- "BFGS"
    args$initial_values <- NULL

    args$same_cov <- FALSE

    fit1 <- do.call(fit_mmrm_multiopt, args = args)
    fit2 <- do.call(fit_mmrm, args = args)

    expect_equal(fit1, fit2)
    expect_valid_fit_object(fit1, "us", 3, FALSE)


    ###### Multiple optimisers
    args$optimizer <- c("BFGS", "Nelder-Mead", "L-BFGS-B")

    fit3 <- do.call(fit_mmrm_multiopt, args = args)

    expect_valid_fit_object(fit3, "us", 3, FALSE)
    expect_equal(fit3, fit2)
})





test_that("MMRM returns expected estimates under different model specifications", {

    set.seed(101)

    sigma <- as_covmat(c(5, 3, 8), c(0.4, 0.6, 0.3))

    dat <- ife(
        is_nightly(),
        get_sim_data(n = 150, sigma),
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







test_that("initial values speed up BFGS", {

    set.seed(315)
    sigma <- as_covmat(c(5, 3, 8), c(0.4, 0.6, 0.3))

    dat <- get_sim_data(n = 200, sigma)

    vars <- ivars(
        subjid = "id",
        covariates = c("group", "sex", "visit * group")
    )

    frm <- as_simple_formula(vars)
    model_df <- as_model_df(dat = dat, frm = frm)

    x <- time_it({
        fit <- fit_mmrm_multiopt(
            designmat = model_df[, -1, drop = FALSE],
            outcome = as.data.frame(model_df)[, 1],
            subjid = dat[[vars$subjid]],
            visit = dat[[vars$visit]],
            group = dat[[vars$group]],
            cov_struct = "us",
            REML = TRUE,
            same_cov = TRUE,
            optimizer = "BFGS"
        )
    })

    y <- time_it({
        fit2 <- fit_mmrm_multiopt(
            designmat = model_df[, -1, drop = FALSE],
            outcome = as.data.frame(model_df)[, 1],
            subjid = dat[[vars$subjid]],
            visit = dat[[vars$visit]],
            group = dat[[vars$group]],
            cov_struct = "us",
            REML = TRUE,
            same_cov = TRUE,
            optimizer = list(
                BFGS = fit[c("beta", "theta")]
            )
        )
    })

    expect_true(
        (as.numeric(x) * 0.5) > as.numeric(y)
    )
})



