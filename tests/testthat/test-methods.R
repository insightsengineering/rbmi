



test_that("methods",{

    expect_error(
        method_condmean(type = "bootstrap"),
        regexp = "n_samples must not be NA"
    )

    expect_error(
        method_condmean(type = "jackknife", n_samples = 20),
        regexp = "n_samples must be NA"
    )

    expect_equal(
        method_condmean(n_samples = 20)$n_samples,
        20
    )

    expect_equal(
        method_condmean(type = "jackknife")$n_samples,
        NA
    )

})
