suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})

test_that("locf", {

    expect_equal(locf(c(NA, 1, 2, NA)), c(NA, 1, 2, 2))
    expect_equal(locf(c(NA, NA, NA)), c(NA, NA, NA))
    expect_equal(locf(c("A", "B", NA, "C", NA, "D")), c("A", "B", "B", "C", "C", "D"))

    val <-  factor(c(NA, 1, 1, NA, 2, NA, 3), labels = c("A", "B", "C"))
    val_expected <- factor(c(NA, 1, 1, 1, 2, 2, 3),  labels = c("A", "B", "C"))
    expect_equal(locf(val), val_expected)

})


test_that("expand", {

    input_df <- dplyr::tibble(
        covar = c("x", "y", "z"),
        pt = c("a", "a", "b"),
        vis = c("1", "2", "2"),
        outcome = c(1, 2, 3)
    )

    df_actual <- expand(
        input_df,
        pt = c("a", "b", "c"),
        vis = c("1", "2", "3")
    )

    df_expected <- dplyr::tibble(
        covar = c("x", "y", NA, NA, "z", NA, NA, NA, NA),
        pt = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c"), levels = c("a", "b", "c")),
        vis = factor(c("1", "2", "3", "1", "2", "3", "1", "2", "3"), levels = c("1", "2", "3")),
        outcome = c(1, 2, NA, NA, 3, NA, NA,  NA, NA)
    )

    expect_equal(df_actual, df_expected)


    df_actual <- expand(
        input_df,
        pt = c("b", "a", "c"),
        vis = c("3", "2", "1")
    )

    df_expected <- dplyr::tibble(
        covar = c(NA, "z", NA, NA, "y", "x", NA, NA, NA),
        pt = factor(c("b", "b", "b", "a", "a", "a", "c", "c", "c"), levels = c("b", "a", "c")),
        vis = factor(c("3", "2", "1", "3", "2", "1", "3", "2", "1"), levels = c("3", "2", "1")),
        outcome = c(NA, 3, NA, NA, 2, 1, NA,  NA, NA)
    )

    expect_equal(df_actual, df_expected)

    expect_error(expand(input_df, zxcv = c("a", "b")), regexp = "`zxcv`")
    expect_error(expand(input_df, pt = c("b", "c", "d")), regexp = "`pt`")
})






test_that("fill_locf", {

    input_df <- dplyr::tibble(
        v1 = c(NA,  1,    2,   NA,  NA, 3,  NA,   4),
        v3 = c(1, 2, NA, 3, 4, 5, 6, 7),
        pt = c("a", "a", "a", "a", "b", "b", "b", "b"),
        v2 = c(1:8)
    )

    df_actual <- fill_locf(
        input_df,
        vars = c("v1", "v2", "v3"),
        group = "pt"
    )

    df_expected <- dplyr::tibble(
        v1 = c(NA,  1,    2,   2,  NA, 3,  3,   4),
        v3 = c(1, 2, 2, 3, 4, 5, 6, 7),
        pt = c("a", "a", "a", "a", "b", "b", "b", "b"),
        v2 = c(1:8)
    )
    expect_equal(df_actual, df_expected)


    df_actual <- fill_locf(
        input_df,
        vars = c("v1", "v2", "v3")
    )

    df_expected <- dplyr::tibble(
        v1 = c(NA,  1,    2,   2,  2, 3,  3,   4),
        v3 = c(1, 2, 2, 3, 4, 5, 6, 7),
        pt = c("a", "a", "a", "a", "b", "b", "b", "b"),
        v2 = c(1:8)
    )
    expect_equal(df_actual, df_expected)





    input_df <- dplyr::tibble(
        v1 = c(NA,  1,    2,   NA,  NA, 3,  NA,   4),
        pt = c("b", "b", "a", "a", "a", "a", "b", "b"),
        v2 = c(1:8)
    )

    df_actual <- fill_locf(
        input_df,
        vars = c("v1", "v2"),
        group = "pt"
    )

    df_expected <- dplyr::tibble(
        v1 = c(NA,  1,    2,   2,  2, 3,  1,   4),
        pt = c("b", "b", "a", "a", "a", "a", "b", "b"),
        v2 = c(1:8)
    )
    expect_equal(df_actual, df_expected)






    input_df <- dplyr::tibble(
        v1 = c(NA,   1,   2,  NA,  NA,  3,   NA,  4),
        pt = c("b", "a", "a", "a", "b", "a", "b", "b"),
        srt1 = c(3,  1,   3,   3,   3,   2,   2,   1),
        srt2 = c(2,  1,   2,   1,   1,   1,   1,   1),
        v2 = c(1:8)
    )

    df_actual <- fill_locf(
        input_df,
        vars = c("v1", "v2"),
        group = "pt",
        order = c("srt1", "srt2")
    )

    df_expected <-  dplyr::tibble(
        v1 = c(4,   1,   2,   3,  4,  3,   4,  4),
        pt = c("b", "a", "a", "a", "b", "a", "b", "b"),
        srt1 = c(3,  1,   3,   3,   3,   2,   2,   1),
        srt2 = c(2,  1,   2,   1,   1,   1,   1,   1),
        v2 = c(1:8)
    )
    expect_equal(df_actual, df_expected)


    input_df <- tibble(
        v1 = 1,
        v2 = 3
    )

    expect_error(fill_locf(input_df, vars = c("v1", "v2", "v3")), regexp = "`v3`")
    expect_error(fill_locf(input_df, vars = c("v1", "v2"), group = "v3"), regexp = "`v3`")
    expect_error(fill_locf(input_df, vars = c("v1", "v2"), order = "v3"), regexp = "`v3`")

})

