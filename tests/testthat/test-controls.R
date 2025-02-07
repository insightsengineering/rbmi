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