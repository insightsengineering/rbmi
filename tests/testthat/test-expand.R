

test_that("locf",{

    expect_equal(locf(c(NA, 1, 2, NA)), c(NA, 1,2,2))
    expect_equal(locf(c(NA,NA, NA)), c(NA,NA, NA))
    expect_equal(locf(c("A", "B", NA, "C", NA, "D")), c("A", "B", "B", "C", "C", "D"))

    val <-  factor( c(NA, 1, 1, NA, 2, NA, 3), labels = c("A", "B", "C"))
    val_expected <- factor( c(NA, 1, 1, 1, 2, 2, 3),  labels = c("A", "B", "C"))
    expect_equal( locf(val), val_expected)

})


test_that("expand",{
    df <- dplyr::tibble(
        pt = c("a", "a", "b"),
        vis = c("1", "2", "1"),
        outcome = c(1,2,3),
        covar = c("x", "y", "z")
    )

    df_expected <- dplyr::tibble(
        pt = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c"), labels = c("a", "b", "c")),
        vis = factor(c("1", "2", "3","1", "2", "3","1", "2", "3"), labels = c("1", "2", "3")),
        outcome = c(1, 2, NA, 3, NA, NA, NA,  NA, NA),
        covar = c("x", "y", "y", "z", "z", "z", NA, NA, NA)
    )

    df_actual <- expand(
        df,
        pt = c("a", "b", "c"),
        vis = c("1", "2", "3") ,
        .fill_vars = "covar",
        .fill_group = "pt"
    )

    expect_equal(df_actual, df_expected)



    df <- dplyr::tibble(
        pt = c("a", "a", "b"),
        vis = c("1", "2", "1"),
        outcome = c(1,NA,3),
        covar = c("x", NA, "z")
    )

    df_actual <- expand(
        df,
        .fill_vars = "covar",
        .fill_group = "pt"
    )

    df_expected <- dplyr::tibble(
        pt = c("a", "a", "b"),
        vis = c("1", "2", "1"),
        outcome = c(1,NA,3),
        covar = c("x", "x", "z")
    )



    expect_equal(df_actual, df_expected)


    df_actual <- expand(
        df,
        .fill_vars = "covar",
        .fill_group = "pt"
    )

    df_expected <- dplyr::tibble(
        pt = c("a", "a", "b"),
        vis = c("1", "2", "1"),
        outcome = c(1,NA,3),
        covar = c("x", "x", "z")
    )




    expect_error(
        expand( df, .fill_vars = "covar2", .fill_group = "pt" ),
        regexp = "covar2"
    )

    expect_error(
        expand( df, .fill_vars = "covar", .fill_group = "pt2" ),
        regexp = "pt2"
    )

    expect_error(
        expand( df, pt2 = c("a", "b", "c")),
        regexp = "`pt2`"
    )

    expect_error(
        expand( df, pt = c("b", "c")),
        regexp = "`pt`"
    )

})


