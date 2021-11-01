suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})


test_that("delta_template & delta_lagscale",{
    set.seed(101)

    n_vis <- 5

    dat <- tibble(
        pt = factor(rep(c("Tom", "Harry"), each = n_vis), levels = c("Tom", "Harry")),
        out = c(1, NA, 3, NA, NA, NA, 5, NA, 6, 7),
        vis = factor(rep(paste0("visit_", 1:n_vis), 2)),
        grp = factor(rep(c("A", "B"), each = n_vis)),
        cov1 = rnorm(n_vis * 2),
        cov2 = rnorm(n_vis * 2)
    )

    vars <- set_vars(
        outcome = "out",
        visit = "vis",
        subjid = "pt",
        group = "grp",
        strata = NULL,
        covariates = c("cov1", "cov2", "cov1*cov2"),
        strategy = "strategy"
    )

    ices <- data.frame(
        pt = c("Tom", "Harry"),
        vis = c("visit_4", "visit_2"),
        strategy = c("JTR", "MAR"),
        stringsAsFactors = FALSE
    )

    longd <- longDataConstructor$new(
        data = dat,
        vars = vars
    )

    longd$set_strategies(ices)

    output_expected <- select(dat, pt, vis, grp) %>%
        mutate(
            is_mar  =     c(T, T, T, F, F,    T, T, T, T, T),
            is_missing =  c(F, T, F, T, T,    T, F, T, F, F),
            is_post_ice = c(F, F, F, T, T,    F, T, T, T, T),
            strategy = c(NA, "MAR", NA, "JTR", "JTR",    "MAR", NA, "MAR", NA, NA),
            delta = rep(0, n_vis * 2)
        ) %>%
        as.data.frame()

    expect_equal(
        delta_template(list(data = longd)),
        output_expected
    )

    dlag <- c(1, 2, 3, 4, 5)
    delta <- c(-3, -6, -6, -12, 1)

    output_expected2 <- output_expected %>%
        mutate(delta = c(0, 0, 0, -12, -10,     0, 0, -18, 0, 0))

    expect_equal(
        delta_template(list(data = longd), delta, dlag),
        output_expected2
    )
})


test_that( "d_lagscale",{

    dlag <- c(1, 1, 1, 1)
    delta <- c(-3, -6, -6, -12)

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

    dlag <- c(0, 1, 1, 1)
    delta <- c(-3, -6, -6, -12)

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


    dlag <- c(1, 0, 0, 0)
    delta <- c(10, 10, 10, 10)

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





test_that("apply_delta", {

    ### Using delta = NULL results in no changes
    d1 <- tibble(
        out = c(1, 2, 3),
        v1 = c(1, 2, 3),
        v2 = c(1, 2, 3),
        id = c("c", "b", "a")
    )
    output_actual <- apply_delta(d1, group = "id", outcome = "out")
    expect_equal(d1, output_actual)

    delta <- tibble()
    output_actual <- apply_delta(d1, delta, group = "id", outcome = "out")
    expect_equal(d1, output_actual)


    ### Changes happen as expected
    delta <- tibble(
        id = c("b", "a", "d"),
        delta = c(1, 2, 3)
    )
    output_expected <- d1
    output_expected$out <- c(1, 3, 5)
    output_actual <- apply_delta(d1, delta, group = "id", outcome = "out")
    expect_equal(output_expected, output_actual, ignore_attr = TRUE)


    ### Changes based on multigroup
    delta <- tibble(
        id = c("b", "a", "d"),
        v1 = c(2, 9, 9),
        delta = c(1, 2, 3)
    )
    output_expected <- d1
    output_expected$out <- c(1, 3, 3)
    output_actual <- apply_delta(d1, delta, group = c("id", "v1"), outcome = "out")
    expect_equal(output_expected, output_actual, ignore_attr = TRUE)


    ### Duplicate IDs in original dataset work as expected
    d1 <- tibble(
        out = c(1, 2, 3, 4),
        v1 = c(1, 1, 1, 2),
        v2 = c(1, 2, 3, 4),
        id = c("c", "b", "a", "b")
    )
    delta <- tibble(
        id = c("b", "a", "d"),
        delta = c(1, 2, 3)
    )
    output_expected <- d1
    output_expected$out <- c(1, 3, 5, 5)
    output_actual <- apply_delta(d1, delta, group = c("id"), outcome = "out")
    expect_equal(output_expected, output_actual, ignore_attr = TRUE)
    expect_error(
        apply_delta(d1, delta, group = c("id", "v1"), outcome = "out"),
        regexp = "`v1` is not in `delta`"
    )

    delta <- tibble(
        id = c("b", "a", "d"),
        v1 = c(1, 2, 2),
        delta = c(1, 2, 3)
    )
    output_expected <- d1
    output_expected$out <- c(1, 3, 3, 4)
    output_actual <- apply_delta(d1, delta, group = c("id", "v1"), outcome = "out")
    expect_equal(output_expected, output_actual, ignore_attr = TRUE)

    delta <- tibble(
        id = c("b", "a", "d", "b"),
        v1 = c(2, 2, 2, 1),
        delta = c(1, 2, 3, 9)
    )
    output_expected <- d1
    output_expected$out <- c(1, 11, 3, 5)
    output_actual <- apply_delta(d1, delta, group = c("id", "v1"), outcome = "out")
    expect_equal(output_expected, output_actual, ignore_attr = TRUE)


    ### Duplicate Ids result in an error
    delta <- tibble(
        id = c("b", "a", "d", "b"),
        delta = c(1, 2, 3, 4)
    )
    expect_error(
        apply_delta(d1, delta, group = "id", outcome = "out"),
        "whilst applying delta"
    )
})



test_that("extract_imputed_dfs + delta", {

    set.seed(301)
    sigma <- as_vcov(c(6, 4, 4), c(0.5, 0.2, 0.3))
    dat <- get_sim_data(50, sigma)

    dat_ice <- dat %>%
        group_by(id) %>%
        arrange(desc(visit)) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(strategy = "MAR")

    vars <- set_vars(
        visit = "visit",
        subjid = "id",
        group = "group",
        covariates = "sex",
        strategy = "strategy",
        outcome = "outcome"
    )

    dobj <- draws(dat, dat_ice, vars = vars, method = method_approxbayes(n_samples = 5))
    iobj <- impute(dobj, c("A" = "B", "B" = "B"))

    delta <- tibble(
        id = c("2", "2"),
        visit = c("visit_3", "visit_1"),
        delta = c(4, 7)
    )

    d1 <- extract_imputed_dfs(iobj, 1)[[1]]
    d2 <- extract_imputed_dfs(iobj, 1, delta = delta)[[1]]

    d1$outcome[c(4, 6)] <- d1$outcome[c(4, 6)]  + c(7, 4)
    expect_equal(d1, d2)

    ### check that extraction / imputation hasn't changed sort order
    d1 <- extract_imputed_dfs(iobj, 1)[[1]] %>%
        select(-id)

    expect_equal(d1, select(dat, -id) %>%  as.data.frame)

})

