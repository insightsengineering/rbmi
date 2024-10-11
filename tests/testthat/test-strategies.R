



test_that("adjusted unconditional covariance matrix for JR and CIR is correct (same_cov = TRUE)", {

    sigma_group <- sigma_ref <- diag(rep(1, 3)) + 0.5
    index_mar <- c(TRUE, TRUE, FALSE)

    expect_equal(compute_sigma(sigma_group, sigma_ref, index_mar), sigma_ref)

})



test_that("adjusted unconditional covariance matrix for JR and CIR is correct (same_cov = FALSE)", {

    sigma_group <- sigma_ref <- diag(rep(1, 3)) + 0.5
    sigma_ref <- diag(rep(1, 3)) + 0.2
    index_mar <- c(TRUE, TRUE, FALSE)

    sigma_strategy <- compute_sigma(sigma_group, sigma_ref, index_mar)

    expect_false(identical(sigma_strategy, sigma_ref))
    expect_false(identical(sigma_strategy, sigma_group))
    expect_false(sigma_strategy[3, 3] == sigma_ref[3, 3])
    expect_equal(sigma_strategy[1:2, 1:2], sigma_group[1:2, 1:2])
    expect_false(identical(sigma_strategy[1:2, 3], sigma_group[1:2, 3]))
    expect_false(identical(sigma_strategy[1:2, 3], sigma_ref[1:2, 3]))
    expect_true(isSymmetric(sigma_strategy))

})



test_that("adjusted unconditional covariance matrix for JR and CIR is correct (all MAR data)", {

    sigma_group <- sigma_ref <- diag(rep(1, 3)) + 0.5
    sigma_ref <- diag(rep(1, 3)) + 0.2
    index_mar <- c(TRUE, TRUE, TRUE)

    expect_equal(compute_sigma(sigma_group, sigma_ref, index_mar), sigma_group)

})



test_that("adjusted unconditional covariance matrix for JR and CIR is correct (all non-MAR data)", {

    sigma_group <- sigma_ref <- diag(rep(1, 3)) + 0.5
    sigma_ref <- diag(rep(1, 3)) + 0.2
    index_mar <- c(FALSE, FALSE, FALSE)

    expect_equal(compute_sigma(sigma_group, sigma_ref, index_mar), sigma_ref)

})



test_that("mean and covariance under CIR are as expected", {

    pars_group <- list(
        mu = c(1, 3, 5),
        sigma = diag(rep(1, 3))
    )
    pars_ref <- list(
        mu = c(2, 6, 10),
        sigma = diag(rep(1, 3))
    )

    index_mar <- c(TRUE, FALSE, FALSE)

    expect_equal(
        strategy_CIR(pars_group, pars_ref, index_mar),
        list(
            mu = c(1, 5, 9),
            sigma = diag(rep(1, 3))
        )
    )

    expect_equal(
        strategy_CIR(pars_group, pars_group, index_mar),
        pars_group
    )

    expect_equal(
        strategy_CIR(pars_group, pars_ref, index_mar = rep(FALSE, 3)),
        pars_ref
    )

    expect_equal(
        strategy_CIR(pars_group, pars_ref, index_mar = rep(TRUE, 3)),
        pars_group
    )

})



test_that("mean and covariance under LMCF are as expected", {

    pars_group <- list(
        mu = c(1, 3, 5),
        sigma = diag(rep(1, 3))
    )
    pars_ref <- list(
        mu = c(2, 6, 10),
        sigma = diag(rep(1, 3))
    )

    index_mar <- c(TRUE, FALSE, FALSE)

    expect_equal(
        strategy_LMCF(pars_group, pars_ref, index_mar),
        list(
            mu = c(1, 1, 1),
            sigma = diag(rep(1, 3))
        )
    )

    expect_error(
        strategy_LMCF(pars_group, pars_ref, index_mar = rep(FALSE, 3))
    )

    expect_equal(
        strategy_LMCF(pars_group, pars_ref, index_mar = rep(TRUE, 3)),
        pars_group
    )

})



test_that("mean and covariance under JR are as expected", {

    pars_group <- list(
        mu = c(1, 3, 5),
        sigma = diag(rep(1, 3))
    )
    pars_ref <- list(
        mu = c(2, 6, 10),
        sigma = diag(rep(1, 3))
    )

    index_mar <- c(TRUE, FALSE, FALSE)

    expect_equal(
        strategy_JR(pars_group, pars_ref, index_mar),
        list(
            mu = c(1, 6, 10),
            sigma = diag(rep(1, 3))
        )
    )

    expect_equal(
        strategy_JR(pars_group, pars_group, index_mar),
        pars_group
    )

    expect_equal(
        strategy_JR(pars_group, pars_ref, index_mar = rep(FALSE, 3)),
        pars_ref
    )

    expect_equal(
        strategy_JR(pars_group, pars_ref, index_mar = rep(TRUE, 3)),
        pars_group
    )

})



test_that("mean and covariance under CR are as expected", {

    pars_group <- list(
        mu = c(1, 3, 5),
        sigma = diag(rep(1, 3))
    )
    pars_ref <- list(
        mu = c(2, 6, 10),
        sigma = diag(rep(1, 3))
    )

    index_mar <- c(TRUE, FALSE, FALSE)

    expect_equal(
        strategy_CR(pars_group, pars_ref, index_mar),
        pars_ref
    )

})






test_that("getStrategies", {

    output_actual <- getStrategies()
    output_expected <- list(
        "JR" = strategy_JR,
        "CR" = strategy_CR,
        "CIR" = strategy_CIR,
        "LMCF" = strategy_LMCF,
        "MAR" = strategy_MAR
    )
    expect_equal(output_actual, output_expected)


    myfun <- function(x) x
    output_actual <- getStrategies(fun = myfun, JR = myfun, MAR = myfun)
    output_expected <- list(
        "JR" = myfun,
        "CR" = strategy_CR,
        "CIR" = strategy_CIR,
        "LMCF" = strategy_LMCF,
        "fun" = myfun,
        "MAR" = strategy_MAR
    )
    expect_equal(output_actual, output_expected)
})
