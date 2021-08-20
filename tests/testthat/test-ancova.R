suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})




test_that("ancova", {

    ##################
    #
    # Basic usage
    #
    #

    set.seed(101)

    n <- 1000
    dat <- tibble(
        visit = "vis1",
        age1 = rnorm(n),
        age2 = rnorm(n),
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2,  sd = 20)
    )

    mod <- lm(out ~ age1 + age2 + grp, data = dat)

    result_expected <- list(
        "trt_vis1" = list(
            "est" = mod$coefficients[[4]],
            "se" = sqrt(vcov(mod)[4, 4]),
            "df" = df.residual(mod)
        )
    )
    result_actual <- ancova(
        dat,
        list(outcome = "out", group = "grp", covariates = c("age1", "age2"), visit = "visit")
    )["trt_vis1"]

    expect_equal(result_expected, result_actual)


    ##################
    #
    # No Covariates
    #
    #

    set.seed(101)

    n <- 1000
    dat <- tibble(
        ivis = " 1",
        age1 = rnorm(n),
        age2 = rnorm(n),
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2,  sd = 20)
    )

    mod <- lm(out ~ grp, data = dat)

    result_expected <- list(
        "trt_ 1" = list(
            "est" = mod$coefficients[[2]],
            "se" = sqrt(vcov(mod)[2, 2]),
            "df" = df.residual(mod)
        )
    )
    result_actual <- ancova(dat, list(outcome = "out", group = "grp", visit = "ivis"))["trt_ 1"]

    expect_equal(result_expected, result_actual)



    ##################
    #
    # Single visit
    #
    #

    n <- 1000
    dat <- tibble(
        age1 = rnorm(n),
        age2 = rnorm(n),
        vis = "visit 1",
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2,  sd = 20)
    )

    mod <- lm(out ~ age1 + age2 + grp, data = dat)

    result_expected <- list(
        "trt_visit 1" = list(
            "est" = mod$coefficients[[4]],
            "se" = sqrt(vcov(mod)[4, 4]),
            "df" = df.residual(mod)
        )
    )

    result_actual <- ancova(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = c("age1", "age2"),
            visit = "vis"
        ),
        visits = "visit 1"
    )["trt_visit 1"]

    expect_equal(result_expected, result_actual)


    ##################
    #
    # Multiple Visits
    #
    #

    n <- 1000
    dat <- tibble(
        age1 = rnorm(n),
        age2 = rnorm(n),
        vis = sample(c("visit 1", "visit 2"), size = n, replace = TRUE),
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2,  sd = 20)
    )

    mod <- lm(out ~ age1 + age2 + grp, data = filter(dat, vis == "visit 1"))

    result_expected <- list(
        "trt_visit 1" = list(
            "est" = mod$coefficients[[4]],
            "se" = sqrt(vcov(mod)[4, 4]),
            "df" = df.residual(mod)
        )
    )

    result_actual <- ancova(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = c("age1", "age2"),
            visit = "vis"
        ),
        visits = "visit 1"
    )["trt_visit 1"]

    expect_equal(result_expected, result_actual)

    result_actual <- ancova(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = c("age1", "age2"),
            visit = "vis"
        ),
        visits = c("visit 1", "visit 2")
    )["trt_visit 1"]

    expect_equal(result_expected, result_actual)

    result_actual <- ancova(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = c("age1", "age2"),
            visit = "vis"
        ),
        visits = c("visit 1", "visit 2")
    )

    mod <- lm(out ~ age1 + age2 + grp, data = filter(dat, vis == "visit 2"))

    result_expected <- list(
        "trt_visit 2" = list(
            "est" = mod$coefficients[[4]],
            "se" = sqrt(vcov(mod)[4, 4]),
            "df" = df.residual(mod)
        )
    )

    expect_equal(result_expected, result_actual["trt_visit 2"])

    expect_equal(
        names(result_actual),
        c(
            "trt_visit 1", "lsm_ref_visit 1", "lsm_alt_visit 1",
            "trt_visit 2", "lsm_ref_visit 2", "lsm_alt_visit 2"
        )
    )

    ##################
    #
    # Visit variable handling
    #
    #

    n <- 1000
    dat <- tibble(
        age1 = rnorm(n),
        age2 = rnorm(n),
        vis = sample(c("visit 1", "visit 2"), size = n, replace = TRUE),
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2,  sd = 20)
    )

    vars <- ivars(
        outcome = "out",
        group = "grp",
        covariates = c("age1", "age2")
    )

    vars$visit <- "vi"

    expect_error(
        ancova(dat, vars, visits = "k"),
        regex = "`vi`"
    )

    vars$visit <- "vis"

    expect_error(
        ancova(dat, vars, visits = "k"),
        regex = "`k`"
    )
})


test_that("least_square_means", {

    ###### Check that treatment effect is unaffected
    set.seed(101)
    n <- 1000
    dat <- tibble(
        age1 = rnorm(n),
        age2 = rnorm(n),
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2,  sd = 20),
        vis = 1
    )

    mod <- lm(out ~ age1 + age2 + grp, data = dat)

    mod2 <- ancova(
        dat,
        list(outcome = "out", group = "grp", covariates = c("age1", "age2"), visit = "vis")
    )

    expect_equal(
        coef(mod)[["grpB"]],
        mod2$lsm_alt_1$est - mod2$lsm_ref_1$est
    )

    ####### Direct comparison to emmeans
    i2 <- iris
    i2[["group"]] <- factor(rep(c("A", "B"), each = 75))
    i2[["group2"]] <- sample(c("A", "B", "C", "D"), size = 150, replace = TRUE)

    mod <- lm(data = i2, Sepal.Length ~ Sepal.Width + Species * group2 + group)

    x <- as.data.frame(emmeans::emmeans(mod, "group"))

    result_expected <- list(
        list(est = x$emmean[[1]], se = x$SE[[1]], df = x$df[[1]]),
        list(est = x$emmean[[2]], se = x$SE[[2]], df = x$df[[2]])
    )

    result_actual <- list(
        lsmeans(mod, group = "A"),
        lsmeans(mod, group = "B")
    )

    expect_equal(result_actual, result_expected)

})



