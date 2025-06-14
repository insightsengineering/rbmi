test_that("control_bayes", {
    set.seed(123)

    result <- control_bayes()
    expect_equal(
        control_bayes(),
        list(
            warmup = 200,
            thin = 50,
            chains = 1,
            init = "mmrm",
            seed = 1756553742
        )
    )
    set.seed(123)
    result <- control_bayes()
    expect_equal(
        control_bayes(verbose = TRUE),
        list(
            warmup = 200,
            thin = 50,
            chains = 1,
            init = "mmrm",
            seed = 1756553742,
            verbose = TRUE
        )
    )
    expect_error(
        control_bayes(n_samples = 1000),
        "specify the number of samples directly"
    )
    expect_error(
        control_bayes(iter = 1000),
        "specify the number of samples directly"
    )
    expect_error(
        control_bayes(refresh = 1000),
        "of the `refresh` argument here"
    )
})

test_that("complete_control_bayes works as expected", {
    control <- control_bayes()
    # We use dummy inputs here for simplicity.
    stan_data <- list(R = matrix(1, nrow = 1, ncol = 1))
    mmrm_initial <- structure(
        list(beta = 1, sigma = 1, tau = 1),
        cov_param_names = c("sigma", "tau")
    )
    result <- complete_control_bayes(
        control = control,
        n_samples = 1000,
        quiet = TRUE,
        stan_data = stan_data,
        mmrm_initial = mmrm_initial
    )
    expect_true(is.list(result))
    expect_true(
        is.list(result$init),
        setequal(names(result$init), c("theta", "sigma", "tau"))
    )
})
