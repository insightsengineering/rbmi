suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
})


test_that("print - Bayes", {
    expect_snapshot(print(.test_print$bayes$draws), cran = TRUE)
    expect_snapshot(print(.test_print$bayes$impute), cran = TRUE)
    expect_snapshot(print(.test_print$bayes$analysis), cran = TRUE)
    expect_snapshot(print(.test_print$bayes$pool), cran = TRUE)
})


test_that("print - Approx Bayes", {
    expect_snapshot(print(.test_print$approxbayes$draws), cran = TRUE)
    expect_snapshot(print(.test_print$approxbayes$impute), cran = TRUE)
    expect_snapshot(print(.test_print$approxbayes$analysis), cran = TRUE)
    expect_snapshot(print(.test_print$approxbayes$pool), cran = TRUE)
})


test_that("print - Condmean Bootstrap", {
    expect_snapshot(print(.test_print$condmean_boot$draws), cran = TRUE)
    expect_snapshot(print(.test_print$condmean_boot$impute), cran = TRUE)
    expect_snapshot(print(.test_print$condmean_boot$analysis), cran = TRUE)
    expect_snapshot(print(.test_print$condmean_boot$pool$percentile), cran = TRUE)
    expect_snapshot(print(.test_print$condmean_boot$pool$normal), cran = TRUE)
})


test_that("print - Condmean Jack", {
    expect_snapshot(print(.test_print$condmean_jack$draws), cran = TRUE)
    expect_snapshot(print(.test_print$condmean_jack$impute), cran = TRUE)
    expect_snapshot(print(.test_print$condmean_jack$analysis), cran = TRUE)
    expect_snapshot(print(.test_print$condmean_jack$pool), cran = TRUE)
})
