



test_that("methods", {

    expect_error(
        method_condmean(type = "bootstrap"),
        regexp = "n_samples must be numeric"
    )

    expect_error(
        method_condmean(type = "jackknife", n_samples = 20),
        regexp = "n_samples must be NULL"
    )

    expect_error(
        method_bmlmi(D = 1),
        regexp = "`D` must be a numeric larger than 1"
    )

    expect_equal(
        method_condmean(n_samples = 20)$n_samples,
        20
    )

    expect_equal(
        method_condmean(type = "jackknife")$n_samples,
        NULL
    )

})
