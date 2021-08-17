



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

    expect_true(all(names(fit) %in% c("beta", "sigma", "converged", "theta")))

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

    expect_true(fit$converged %in% c(TRUE, FALSE))
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
        converged = converged
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
        initial_values = NULL,
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


