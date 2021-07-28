

suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
})



test_that("as_model_df", {

    x <- tibble(
        x = c(NA, 1, 2, 3),
        y = c(5, 6, 7, 8),
        z = c("A", "B", "C", "A"),
        w = c("A", "A", "B", "B")
    )

    actual_output <- as_model_df(x, x ~ y + z * w)

    expected_output <- tibble(
        v1 = c(NA, 1, 2, 3),
        v2 = c(1, 1, 1, 1),
        v3 = c(5, 6, 7, 8),
        v4 = c(0, 1, 0, 0),
        v5 = c(0, 0, 1, 0),
        v6 = c(0, 0, 1, 1),
        v7 = c(0, 0, 0, 0),
        v8 = c(0, 0, 1, 0)
    )
    colnames(expected_output) <- c("x", "(Intercept)", "y", "zB", "zC", "wB", "zB:wB", "zC:wB")
    rownames(expected_output) <- NULL
    rownames(actual_output) <- NULL

    expect_equal(actual_output, expected_output)

    i2 <- iris
    i2[c(1, 2, 3), "Sepal.Length"] <- NA
    i2[c(4, 5, 6), "Sepal.Length"] <- Inf
    x <- as_model_df(i2, Sepal.Length ~ Sepal.Width * Species)
    expect_true(nrow(x) == nrow(i2))

    i3 <- i2
    i3["Sepal.Width", c(1, 2, 3)] <- NA
    expect_error(as_model_df(i3, Sepal.Length ~ Sepal.Width * Species))
})

