suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
})


compute_n_params <- function(cov_struct, nv) {
    n_params <- switch(
        cov_struct,
        "ad" = nv,
        "adh" = 2 * nv - 1,
        "ar1" = 2,
        "ar1h" = nv + 1,
        "cs" = 2,
        "csh" = nv + 1,
        "toep" = nv,
        "toeph" = 2 * nv - 1,
        "us" = nv * (nv + 1) / 2
    )
    return(n_params)
}


# function for checking whether x is a formula object
is.formula <- function(x) {
    is.call(x) && x[[1]] == quote(`~`)
}


expect_valid_fit_object <- function(fit, cov_struct, nv, same_cov) {
    expect_type(fit, "list")
    expect_true(length(fit) >= 3)

    expect_true(all(names(fit) %in% c("beta", "sigma", "sd", "rho", "failed")))

    expect_vector(fit$beta)
    expect_length(fit$beta, 8)

    expect_type(fit$sigma, "list")
    expect_length(fit$sigma, 2)
    expect_true(is.matrix(fit$sigma[[1]]))
    expect_equal(dim(fit$sigma[[1]]), c(nv, nv))
    expect_true(is.matrix(fit$sigma[[2]]))
    expect_equal(dim(fit$sigma[[2]]), c(nv, nv))

    expect_true(fit$failed %in% c(TRUE, FALSE))
}


extract_test_fit <- function(mod) {
    beta <- coef(mod)
    names(beta) <- NULL

    sigma <- mod$cov
    if (!is.list(sigma)) {
        sigma <- list(sigma)
    }

    sigma <- lapply(sigma, function(x) {
        assert_that(is.matrix(x))
        rownames(x) <- NULL
        colnames(x) <- NULL
        return(x)
    })
    sigma <- ife(
        length(sigma) == 1,
        list("A" = sigma[[1]], "B" = sigma[[1]]),
        list("A" = sigma[[1]], "B" = sigma[[2]])
    )

    converged <- attr(mod, "converged")

    output_expected <- structure(
        list(
            beta = beta,
            sigma = sigma,
            failed = !converged
        ),
        cov_param_names = "sigma"
    )
    return(output_expected)
}


test_mmrm_numeric <- function(dat, formula_expr, same_cov, scale = FALSE) {
    formula <- as.formula(formula_expr)
    designmat <- as_model_df(dat, formula)

    if (scale) {
        dat_limit <- ceiling(nrow(dat) / 2)
        scaler <- scalerConstructor$new(designmat[seq_len(dat_limit), ])
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
        same_cov = same_cov
    )

    if (scale) {
        fit_actual$beta <- scaler$unscale_beta(fit_actual$beta)
        fit_actual$sigma <- lapply(fit_actual$sigma, function(x) {
            scaler$unscale_sigma(x)
        })
    }

    covariance <- ife(
        same_cov,
        " + us(visit | id)",
        " + us(visit | group / id)"
    )

    mod <- mmrm::mmrm(
        as.formula(paste0(formula_expr, covariance)),
        data = dat,
        reml = TRUE,
        n_cores = 1,
        accept_singular = FALSE
    )

    fit_expected <- extract_test_fit(mod)
    if (scale) {
        expect_true(all(
            abs(fit_actual$beta - fit_expected$beta) < 0.001
        ))
        expect_true(all(
            abs(fit_actual$sigma[["A"]] - fit_expected$sigma[["A"]]) < 0.01
        ))
        expect_true(all(
            abs(fit_actual$sigma[["B"]] - fit_expected$sigma[["B"]]) < 0.01
        ))
    } else {
        expect_equal(fit_actual, fit_expected)
    }
}


set.seed(101)

sigma <- as_vcov(c(5, 3, 8), c(0.4, 0.6, 0.3))

dat <- get_sim_data(n = 40, sigma) %>%
    mutate(outcome = if_else(rbinom(n(), 1, 0.2) == 1, NA_real_, outcome))

vars <- set_vars(
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
    same_cov = TRUE
)


test_that("as_mmrm_df & as_mmrm_formula", {
    sigma <- as_vcov(c(2, 6, 3), c(0.4, 0.7, 0.5))
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
    frm_expected <- outcome ~
        V1 + V2 + V3 + V4 + V5 + V6 + us(visit | subjid) - 1
    expect_equal(frm_expected, frm_actual, ignore_attr = TRUE)

    #### With Groupings
    x <- as_mmrm_df(
        designmat = dat,
        outcome = dat$outcome,
        visit = dat$visit,
        subjid = dat$id,
        group = sample(c("A", "B", "C"), size = nrow(x), replace = TRUE)
    )

    expect_equal(ncol(x), ncol(dat) + 4)
    expect_equal(
        colnames(x),
        c(
            paste0("V", seq_len(ncol(dat))),
            "outcome",
            "visit",
            "subjid",
            "group"
        )
    )
    expect_equal(nrow(x), nrow(dat))

    frm_actual <- as_mmrm_formula(x, "us")
    frm_expected <- outcome ~
        V1 + V2 + V3 + V4 + V5 + V6 + us(visit | group / subjid) - 1
    expect_equal(frm_expected, frm_actual, ignore_attr = TRUE)

    frm_actual <- as_mmrm_formula(x, "toep")
    frm_expected <- outcome ~
        V1 + V2 + V3 + V4 + V5 + V6 + toep(visit | group / subjid) - 1
    expect_equal(frm_expected, frm_actual, ignore_attr = TRUE)
    expect_error(as_mmrm_formula(x, "toep2"), regexp = "'arg' should be one of")
})


test_that("MMRM model fit has expected output structure", {
    for (struct in c(
        "ad",
        "adh",
        "ar1",
        "ar1h",
        "cs",
        "csh",
        "toep",
        "toeph",
        "us"
    )) {
        ## First for same covariance per group
        args <- args_default
        args$cov_struct <- struct
        fit <- do.call(fit_mmrm, args = args)
        expect_valid_fit_object(fit, struct, 3, TRUE)

        mod <- mmrm::mmrm(
            formula = as.formula(sprintf(
                "outcome ~ sex + age + visit * group + %s(visit | id)",
                struct
            )),
            data = dat,
            reml = TRUE
        )
        expect_equal(length(mod$theta_est), compute_n_params(struct, 3))
        expect_equal(fit$beta, coef(mod), ignore_attr = TRUE)
        expect_equal(fit$sigma[[1]], VarCorr(mod), ignore_attr = TRUE)

        ## And again for difference covariance per group
        args <- args_default
        args$cov_struct <- struct
        args$same_cov <- FALSE
        fit <- do.call(fit_mmrm, args = args)
        expect_valid_fit_object(fit, struct, 3, FALSE)

        mod <- mmrm::mmrm(
            formula = as.formula(sprintf(
                "outcome ~ sex + age + visit * group + %s(visit | group / id)",
                struct
            )),
            data = dat,
            reml = TRUE
        )
        expect_equal(length(mod$theta_est), compute_n_params(struct, 3) * 2)
        expect_equal(fit$beta, coef(mod), ignore_attr = TRUE)
        expect_equal(fit$sigma, VarCorr(mod), ignore_attr = TRUE)
    }
})


test_that("MMRM model fit has expected output structure (same_cov = FALSE)", {
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

    mod <- mmrm::mmrm(
        outcome ~ sex + age + visit * group + us(visit | id),
        data = dat,
        reml = TRUE,
        n_cores = 1,
        accept_singular = FALSE
    )
    expect_equal(fit, extract_test_fit(mod))
})


test_that("MMRM returns expected estimates (same_cov = FALSE)", {
    args <- args_default
    args$same_cov <- FALSE
    fit <- do.call(fit_mmrm, args = args)

    mod <- mmrm::mmrm(
        outcome ~ sex + age + visit * group + us(visit | group / id),
        data = dat,
        reml = TRUE,
        n_cores = 1,
        accept_singular = FALSE
    )

    expect_equal(fit, extract_test_fit(mod))
})


test_that("MMRM returns expected estimates under different model specifications", {
    testthat::skip_on_cran()

    set.seed(4101)

    sigma <- as_vcov(c(5, 3, 8), c(0.4, 0.6, 0.3))

    dat <- ife(
        is_full_test(),
        get_sim_data(n = 200, sigma),
        get_sim_data(n = 100, sigma)
    )

    dat <- dat %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.2) == 1, NA_real_, outcome))

    runtests <- function(same_cov, scale) {
        formula_expr <- "outcome ~ sex*visit + age*visit + visit*group"
        test_mmrm_numeric(dat, formula_expr, same_cov, scale)

        formula_expr <- "outcome ~ age:sex^2 + sex:age*group + visit*group"
        test_mmrm_numeric(dat, formula_expr, same_cov, scale)

        if (is_full_test()) {
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
    runtests(FALSE, FALSE)
    runtests(FALSE, TRUE)
})


test_that("visit & group factor levels / order doesn't break model extraction", {
    set.seed(3812)
    bign <- 120
    sigma <- as_vcov(
        c(2, 1, 0.7),
        c(
            0.3,
            0.4,
            0.2
        )
    )

    dat <- get_sim_data(bign, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(
            outcome = if_else(
                is_miss == 1 & visit == "visit_3",
                NA_real_,
                outcome
            )
        ) %>%
        select(-is_miss)

    mod <- mmrm::mmrm(
        formula = outcome ~ age + group + sex + visit + us(visit | group / id),
        data = dat
    )
    expected <- mod$cov[["A"]]
    rownames(expected) <- NULL
    colnames(expected) <- NULL
    expect_equal(expected, extract_params(mod)$sigma$A)

    dat_modified <- dat %>%
        mutate(group = relevel(group, "B"))

    mod <- mmrm::mmrm(
        formula = outcome ~ age + group + sex + visit + us(visit | group / id),
        data = dat_modified
    )
    expected <- mod$cov[["A"]]
    rownames(expected) <- NULL
    colnames(expected) <- NULL
    expect_equal(expected, extract_params(mod)$sigma$A)

    dat_modified <- dat %>%
        mutate(visit = relevel(visit, "visit_3"))

    mod <- mmrm::mmrm(
        formula = outcome ~ age + group + sex + visit + us(visit | group / id),
        data = dat_modified
    )
    expected <- mod$cov[["A"]]
    rownames(expected) <- NULL
    colnames(expected) <- NULL
    expect_equal(expected, extract_params(mod)$sigma$A)
})

test_that("extract_params works with different covariance structures", {
    set.seed(1234)
    sigma <- as_vcov(c(2, 1, 0.7), c(0.3, 0.4, 0.2))
    dat <- get_sim_data(n = 100, sigma)

    # Same cov across groups.
    mod <- mmrm::mmrm(
        formula = outcome ~ age + group + sex + visit + ar1(visit | id),
        data = dat
    )
    result <- extract_params(mod)
    expect_true(is.list(result$sd) && length(result$sd) == 1)
    expect_true(is.list(result$rho) && length(result$rho) == 1)

    # Separate cov for each group.
    mod <- mmrm::mmrm(
        formula = outcome ~ age + group + sex + visit + ar1(visit | group / id),
        data = dat
    )
    result <- extract_params(mod)
    expect_true(is.list(result$sd) && length(result$sd) == 2)
    expect_true(is.list(result$rho) && length(result$rho) == 2)
})

test_that("fit_mmrm works with ar1 structure", {
    set.seed(1234)
    sigma <- as_vcov(c(2, 1, 0.7), c(0.3, 0.4, 0.2))
    dat <- get_sim_data(n = 100, sigma)

    mat <- model.matrix(
        data = dat,
        ~ 1 + sex + age + group + visit + group * visit
    )

    # Same cov across groups.
    result <- fit_mmrm(
        designmat = mat,
        outcome = dat$outcome,
        subjid = dat$id,
        visit = dat$visit,
        group = dat$group,
        cov_struct = "ar1",
        REML = TRUE,
        same_cov = TRUE
    )

    expect_true(is.list(result$sigma) && length(result$sigma) == 2)
    expect_true(is.list(result$sd) && length(result$sd) == 2)
    expect_true(is.list(result$rho) && length(result$rho) == 2)

    # Separate cov per group.
    result <- fit_mmrm(
        designmat = mat,
        outcome = dat$outcome,
        subjid = dat$id,
        visit = dat$visit,
        group = dat$group,
        cov_struct = "ar1",
        REML = TRUE,
        same_cov = FALSE
    )

    expect_true(
        is.list(result$sigma) &&
            length(result$sigma) == 2 &&
            identical(names(result$sigma), c("A", "B"))
    )
    expect_true(
        is.list(result$sd) &&
            length(result$sd) == 2 &&
            identical(names(result$sigma), c("A", "B"))
    )
    expect_true(
        is.list(result$rho) &&
            length(result$rho) == 2 &&
            identical(names(result$sigma), c("A", "B"))
    )
})
