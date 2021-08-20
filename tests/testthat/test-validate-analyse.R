suppressPackageStartupMessages({
    library(dplyr)
})

test_that("validate_analyse", {
    res1 <- list(
        list(
            "trt1" = list("est" = 1, "se" = 1, "df" = NA),
            "trt2" = list("est" = 2, "se" = 1, "df" = NA)
        ),
        list(
            "trt1" = list("est" = 3, "se" = 1, "df" = NA),
            "trt2" = list("est" = 4, "se" = 1, "df" = NA)
        )
    )

    expect_true(validate_analyse(res1 %>% as_class("jackknife")))
    expect_true(validate_analyse(res1 %>% as_class("bootstrap")))
    expect_true(validate_analyse(res1 %>% as_class("rubin")))

    expect_error(validate_analyse(res1), regex = "of class")
    expect_error(validate_analyse(res1) %>% as_class("test"), regex = "of class")


    res2 <- list(
        list(
            "trt1" = list("est" = 1),
            "trt2" = list("est" = 2)
        ),
        list(
            "trt1" = list("est" = 3),
            "trt2" = list("est" = 4)
        )
    )

    expect_true(validate_analyse(res2 %>% as_class("jackknife")))
    expect_true(validate_analyse(res2 %>% as_class("bootstrap")))
    expect_error(validate_analyse(res2 %>% as_class("rubin")), regex = "`est`, `se`, `df`")



    res3 <- list(
        list(
            "trt1" = list("est" = 1),
            "trt2" = list("est" = 2)
        ),
        list(
            "trt1" = list("est" = 3),
            "trt3" = list("est" = 4)
        )
    )

    expect_error(validate_analyse(res3 %>% as_class("jackknife")), regex = "identically named")
    expect_error(validate_analyse(res3 %>% as_class("bootstrap")), regex = "identically named")
    expect_error(validate_analyse(res3 %>% as_class("rubin")), regex = "identically named")


    res4 <- list(
        list(
            "a" = list("est" = 1),
            "b" = list("est" = 2)
        ),
        list(
            "trt" = list("est" = 3),
            list("est" = 4)
        )
    )

    expect_error(validate_analyse(res4 %>% as_class("jackknife")), regex = "named lists")
    expect_error(validate_analyse(res4 %>% as_class("bootstrap")), regex = "named lists")
    expect_error(validate_analyse(res4 %>% as_class("rubin")), regex = "named lists")
})


