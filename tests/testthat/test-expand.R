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
    expect_warning(
        df_actual <- fill_locf(
            input_df,
            vars = c("v1", "v2", "v3"),
            group = "pt"
        ),
        regexp = "`v1`"
    )
    df_expected <- dplyr::tibble(
        v1 = c(NA,  1,    2,   2,  NA, 3,  3,   4),
        v3 = c(1, 2, 2, 3, 4, 5, 6, 7),
        pt = c("a", "a", "a", "a", "b", "b", "b", "b"),
        v2 = c(1:8)
    )
    expect_equal(df_actual, df_expected)



    input_df <- dplyr::tibble(
        v1 = c(NA,  1,    2,   NA,  NA, 3,  NA,   4),
        v3 = c(1, 2, NA, 3, 4, 5, 6, 7),
        pt = c("a", "a", "a", "a", "b", "b", "b", "b"),
        v2 = c(1:8)
    )
    expect_warning(
        df_actual <- fill_locf(
            input_df,
            vars = c("v1", "v2", "v3")
        ),
        regexp = "`v1`"
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
    expect_warning(
        df_actual <- fill_locf(
            input_df,
            vars = c("v1", "v2"),
            group = "pt"
        ),
        regexp = "`v1`"
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




    input_df <- dplyr::tibble(
        v1 = c(NA,  1,    2,   2,  2, 3,  3,   4),
        v3 = c(1, 2, 2, 3, 4, 5, 6, 7),
        pt = c(NA, "a", "a", "a", "b", "b", "b", "b"),
        c1 = c(NA, 2:8),
        v2 = c(1:8)
    )
    expect_warning(
        df_actual <- fill_locf(
            input_df,
            vars = c("v1", "v2", "c1"),
            group = "pt",
            order = c("pt", "c1")
        ),
        regexp = "`v1`, `c1`"
    )
    df_expected <- dplyr::tibble(
        v1 = c(NA,  1,    2,   2,  2, 3,  3,   4),
        v3 = c(1, 2, 2, 3, 4, 5, 6, 7),
        pt = c(NA, "a", "a", "a", "b", "b", "b", "b"),
        c1 = c(NA, 2:8),
        v2 = c(1:8)
    )
    expect_equal(df_actual, df_expected)
})




test_that("expand_locf", {

    input_df <- dplyr::tibble(
        c1 = c("A", "B", "A", "C"),
        c2 = c("A", "A", "B", "B"),
        v1 = c(1, 2, 3, 4),
        v2 = c("X", "Y", "Z", "W")
    )

    df_expected <- dplyr::tibble(
        c1 = factor(c("A", "B", "C", "A", "B", "C")),
        c2 = factor(c("A", "A", "A", "B", "B", "B")),
        v1 = c(1, 2, NA, 3, NA, 4),
        v2 = c("X", "Y", "Y", "Z", "Z", "W")
    ) %>%
        arrange(c1, c2)

    df_actual_1 <- input_df %>%
        expand(c1 = c("A", "B", "C"), c2 = c("A", "B")) %>%
        fill_locf(
            vars = "v2",
            group = c("c2"),
            order = c("c1", "c2")
        )

    df_actual_2 <- expand_locf(
        input_df,
        c1 = c("A", "B", "C"),
        c2 = c("A", "B"),
        vars =  "v2",
        group = "c2",
        order = c("c1", "c2")
    )


    expect_equal(df_actual_1, df_expected)
    expect_equal(df_actual_2, df_expected)
})




test_that("fill_locf - works with data.frames & Dates", {
    input_df <- data.frame(
        v1 = c(NA, 1, 2, NA, NA, 3, NA, 4),
        pt = c("b", "a", "a", "a", "b", "a", "b", "b"),
        srt1 = c(3, 1, 3, 3, 3, 2, 2, 1),
        srt2 = c(2, 1, 2, 1, 1, 1, 1, 1),
        mydate = lubridate::ymd("2020:04:30") + lubridate::days(c(1:8) * 25),
        v2 = c(1:8)
    )

    input_df[c(1, 7), "mydate"] <- NA

    df_actual <- fill_locf(
        input_df,
        vars = c("v1", "v2", "mydate"),
        group = "pt",
        order = c("srt1", "srt2")
    )

    df_expected <-  data.frame(
        v1 = c(4,   1,   2,   3,  4,  3,   4,  4),
        pt = c("b", "a", "a", "a", "b", "a", "b", "b"),
        srt1 = c(3,  1,   3,   3,   3,   2,   2,   1),
        srt2 = c(2,  1,   2,   1,   1,   1,   1,   1),
        mydate = lubridate::ymd(
            "2020-09-02", "2020-06-19", "2020-07-14", "2020-08-08",
            "2020-09-02", "2020-09-27", "2020-11-16", "2020-11-16"
        ),
        v2 = c(1:8)
    )

    expect_equal(df_actual, df_expected)
})





test_that("fill_locf - works with list columns", {
    input_df <- dplyr::tibble(
        pt = c("a", "a", "b", "b", "b"),
        srt1 = c(1, 2, 1, 2, 3),
        list_val = list(c(1, 1), NA, NA, c(4, 4, 4, 4), NA)
    )
    expect_warning(
        df_actual <- fill_locf(
            input_df,
            vars = "list_val",
            group = "pt",
            order = "srt1"
        ),
        regexp = "`list_val`"
    )

    df_expected <- dplyr::tibble(
        pt = c("a", "a", "b", "b", "b"),
        srt1 = c(1, 2, 1, 2, 3),
        list_val = list(c(1, 1), c(1, 1), NA, c(4, 4, 4, 4), c(4, 4, 4, 4))
    )
    expect_equal(df_actual, df_expected)



    mod1 <- lm(data = iris, Sepal.Width ~ Sepal.Length)
    input_df <- dplyr::tibble(
        pt = c("a", "a", "b", "b", "b", "b", "b"),
        srt1 = c(1, 2, 1, 2, 3, 4, 5),
        list_val = list(mod1, mod1, mod1, NA, iris, NA, NA)
    )
    df_actual <- fill_locf(
        input_df,
        vars = "list_val",
        group = "pt",
        order = "srt1"
    )
    df_expected <- dplyr::tibble(
        pt = c("a", "a", "b", "b", "b", "b", "b"),
        srt1 = c(1, 2, 1, 2, 3, 4, 5),
        list_val = list(mod1, mod1, mod1, mod1, iris, iris, iris)
    )
    expect_equal(df_actual, df_expected)
})



test_that("fill_locf works with factors", {

    input_df <- data.frame(
        v1 = c(NA, 1, 2, NA, NA, 3, NA, 4),
        pt = c("a", "a", "a", "a", "b", "b", "b", "b"),
        srt1 = c(1, 1, 1, 1, 2, 2, 2, 2),
        srt2 = c(1, 2, 3, 4, 1, 2, 3, 4),
        myfac1 = factor(c("A", "A", NA, "B", NA, "A", "B", NA), levels = c("A", "B", "C")),
        v2 = c(1, 2, 3, 4, NA, 6, NA, 8)
    )

    expect_warning(
        df_actual <- fill_locf(
            input_df,
            vars = c("v1", "myfac1"),
            group = "pt",
            order = c("srt1", "srt2")
        ),
        regexp = "`v1`, `myfac1`"
    )


    df_expected <- data.frame(
        v1 = c(NA, 1, 2, 2, NA, 3, 3, 4),
        pt = c("a", "a", "a", "a", "b", "b", "b", "b"),
        srt1 = c(1, 1, 1, 1, 2, 2, 2, 2),
        srt2 = c(1, 2, 3, 4, 1, 2, 3, 4),
        myfac1 = factor(c("A", "A", "A", "B", NA, "A", "B", "B"), levels = c("A", "B", "C")),
        v2 = c(1, 2, 3, 4, NA, 6, NA, 8)
    )

    expect_equal(df_actual, df_expected)



    # Making sure there's no complications with factor levels only occurring within certain groups
    input_df <- data.frame(
        v1 = c(NA, 1, 2, NA, NA, 3, NA, 4),
        pt = c("a", "a", "a", "a", "b", "b", "b", "b"),
        srt1 = c(1, 1, 1, 1, 2, 2, 2, 2),
        srt2 = c(1, 2, 3, 4, 1, 2, 3, 4),
        myfac1 = factor(c("A", "A", NA, NA, NA, "B", "C", NA), levels = c("A", "B", "C", "D")),
        v2 = c(1, 2, 3, 4, NA, 6, NA, 8)
    )

    expect_warning(
        df_actual <- fill_locf(
            input_df,
            vars = c("v1", "myfac1"),
            group = "pt",
            order = c("srt1", "srt2")
        ),
        regexp = "`v1`, `myfac1`"
    )

    df_expected <- data.frame(
        v1 = c(NA, 1, 2, 2, NA, 3, 3, 4),
        pt = c("a", "a", "a", "a", "b", "b", "b", "b"),
        srt1 = c(1, 1, 1, 1, 2, 2, 2, 2),
        srt2 = c(1, 2, 3, 4, 1, 2, 3, 4),
        myfac1 = factor(c("A", "A", "A", "A", NA, "B", "C", "C"), levels = c("A", "B", "C", "D")),
        v2 = c(1, 2, 3, 4, NA, 6, NA, 8)
    )

    expect_equal(df_actual, df_expected)

})
