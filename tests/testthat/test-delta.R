

suppressPackageStartupMessages(library(dplyr))

dat <- structure(
    list(

        subjid = c(
            "Tom", "Tom", "Tom",
            "Harry", "Harry", "Harry",
            "Phil", "Phil", "Phil",
            "Ben", "Ben", "Ben"
        ),

        age = c(
             0.04, 0.04, 0.04,
            -0.14, -0.14, 0.14,
            -0.03, -0.03, -0.03,
            -0.33, -0.33, -0.33
        ),

        group = structure(
            c(2L, 2L, 2L,
              2L, 2L, 2L,
              1L, 1L, 1L,
              1L, 1L, 1L),
            .Label = c("A", "B"),
            class = "factor"
        ),

        sex = structure(
            c(2L, 2L, 2L,
              1L, 1L, 1L,
              1L, 1L, 1L,
              2L, 2L, 2L),
            .Label = c("M", "F"),
            class = "factor"
        ),

        strata = c(
            "A", "A", "A",
            "A", "A", "A",
            "A", "A", "A",
            "B", "B", "B"
        ),

        outcome = c(
            NA, NA, NA,
            NA, 4.14, NA,
            NA, -1.34, 2.41,
            -1.53, 1.03, 2.58
        ),

        visit = structure(
            c(
                1L, 2L, 3L,
                1L, 2L, 3L,
                1L, 2L, 3L,
                1L, 2L, 3L
            ),
            .Label = c("Visit 1", "Visit 2", "Visit 3"),
            class = "factor")
    ),
    row.names = c(NA,  -12L),
    class = c("tbl_df", "tbl", "data.frame")
)


vars <- list(
    outcome = "outcome",
    visit = "visit",
    subjid = "subjid",
    group = "group",
    strata = "strata",
    covariates = c("sex", "age"),
    method = "method"
)


ices <- data.frame(
    subjid = c("Tom", "Harry"),
    visit = c("Visit 2", "Visit 3"),
    method = c("JTR", "MAR")
)


ld <- longDataConstructor$new(
    data = dat,
    vars = vars
)


ld$set_strategies(ices)


imputations <- list(longdata = ld)

delta_lag_scale(
    imputations,
    visit_delta = c(1, 2, 3), 
    lag_scale = c(1, 2, 3)
)

d_lagscale(
    visit_delta = c(3, 3, 3, 3), 
    lag_scale = c(1, 1, 1, 1), 
    is_post_ice = c(T, T, T, T)
)







test_that("apply_delta",{
    
    d1 <- tibble(
        out = c(1, 2, 3),
        v1 = c(1, 2, 3),
        v2 = c(1, 2, 3),
        id = c("a", "b", "c")
    )
    output_actual <- apply_delta(d1, group = "id", outcome = "out")
    expect_equal(d1, output_actual)
    
    delta <- tibble()
    output_actual <- apply_delta(d1, delta, group = "id", outcome = "out")
    expect_equal(d1, output_actual)
    
    delta <- tibble(
        id = c("b", "a", "d"),
        delta = c(1, 2, 3)
    )
    
    output_expected <- d1
    output_expected$out <- c(3, 3, 3)
    output_actual <- apply_delta(d1, delta, group = "id", outcome = "out")
    expect_equal(output_expected, output_actual)
})



