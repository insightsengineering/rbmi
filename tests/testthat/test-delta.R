suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})


test_that("delta_template & delta_lagscale",{
    set.seed(101)

    n_vis <- 4

    dat <- tibble(
        pt = rep(c("Tom", "Harry"), each= 4),
        out = c(1,NA,3,NA,  NA,5,NA,NA),
        vis = factor(rep(paste0("visit_", 1:n_vis), 2)),
        grp = factor(rep(c("A", "B"), each = 4)),
        cov1 = rnorm(8),
        cov2 = rnorm(8)
    )

    vars <- list(
        outcome = "out",
        visit = "vis",
        subjid = "pt",
        group = "grp",
        strata = NULL,
        covariates = c("cov1", "cov2", "cov1*cov2"),
        method = "method"
    )

    ices <- data.frame(
        pt = c("Tom", "Harry"),
        vis = c("visit_4", "visit_3"),
        method = c("JTR", "MAR"),
        stringsAsFactors = FALSE
    )

    ld <- longDataConstructor$new(
        data = dat,
        vars = vars
    )

    ld$set_strategies(ices)

    output_expected <- select(dat, pt, vis, grp) %>%
        mutate(
            is_mar  = c(T,T,T,F,   T,T,T,T),
            is_missing = c(F,T,F,T, T,F,T,T),
            is_post_ice = c(F,F,F,T,  F,F,T,T),
            strategy = c(NA, "MAR", NA, "JTR",    "MAR", NA , "MAR", "MAR" ),
            delta = rep(0, 8)
        ) %>%
        as.data.frame()

    expect_equal(
        delta_template(list(data = ld)),
        output_expected
    )

    dlag <- c(1, 2, 3, 4)
    delta <- c(-3, -6, -6, -12)

    output_expected2 <- output_expected %>%
        mutate(delta = c(0, 0, 0, -12, 0, 0, -6, -24 -6))

    expect_equal(
        delta_lagscale(list(data = ld), delta, dlag),
        output_expected2
    )
})


test_that( "d_lagscale",{

    dlag = c(1, 1, 1, 1)
    delta = c(-3, -6, -6, -12)

    expect_equal(
        d_lagscale(delta, dlag, c(TRUE, TRUE, TRUE, TRUE)),
        c(-3, -9, -15, -27)
    )
    expect_equal(
        d_lagscale(delta, dlag, c(FALSE, TRUE, TRUE, TRUE)),
        c(0, -6, -12, -24)
    )
    expect_equal(
        d_lagscale(delta, dlag, c(FALSE, FALSE, TRUE, TRUE)),
        c(0, 0, -6, -18)
    )
    expect_equal(
        d_lagscale(delta, dlag, c(FALSE, FALSE, FALSE, TRUE)),
        c(0, 0, 0, -12)
    )

    dlag = c(0, 1, 1, 1)
    delta = c(-3, -6, -6, -12)

    expect_equal(
        d_lagscale(delta, dlag, c(TRUE, TRUE, TRUE, TRUE)),
        c(0, -6, -12, -24)
    )
    expect_equal(
        d_lagscale(delta, dlag, c(FALSE, TRUE, TRUE, TRUE)),
        c(0, 0, -6, -18)
    )
    expect_equal(
        d_lagscale(delta, dlag, c(FALSE, FALSE, TRUE, TRUE)),
        c(0, 0, 0, -12)
    )
    expect_equal(
        d_lagscale(delta, dlag, c(FALSE, FALSE, FALSE, TRUE)),
        c(0, 0, 0, 0)
    )


    dlag = c(1, 0, 0, 0)
    delta = c(10, 10, 10, 10)

    expect_equal(
        d_lagscale(delta, dlag, c(TRUE, TRUE, TRUE, TRUE)),
        c(10, 10, 10, 10)
    )
    expect_equal(
        d_lagscale(delta, dlag, c(FALSE, TRUE, TRUE, TRUE)),
        c(0, 10, 10, 10)
    )
    expect_equal(
        d_lagscale(delta, dlag, c(FALSE, FALSE, TRUE, TRUE)),
        c(0, 0, 10, 10)
    )
    expect_equal(
        d_lagscale(delta, dlag, c(FALSE, FALSE, FALSE, TRUE)),
        c(0, 0, 0, 10)
    )


})





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
