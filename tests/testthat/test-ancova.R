

suppressPackageStartupMessages({
    library(dplyr)
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
        age1 = rnorm(n),
        age2 = rnorm(n),
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2,  sd = 20)
    )

    mod <- lm(out ~ age1 + age2 + grp, data = dat)

    result_expected <- list(
        "trt" = list(
            "est" = mod$coefficients[[4]],
            "se" = sqrt(vcov(mod)[4, 4]),
            "df" = df.residual(mod)
        )
    )
    result_actual <- ancova(
        dat,
        list(outcome = "out", group = "grp", covariates = c("age1", "age2"))
    )["trt"]

    expect_equal(result_expected, result_actual)


    ##################
    #
    # No Covariates
    #
    #

    set.seed(101)

    n <- 1000
    dat <- tibble(
        age1 = rnorm(n),
        age2 = rnorm(n),
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2,  sd = 20)
    )

    mod <- lm(out ~ grp, data = dat)

    result_expected <- list(
        "trt" = list(
            "est" = mod$coefficients[[2]],
            "se" = sqrt(vcov(mod)[2, 2]),
            "df" = df.residual(mod)
        )
    )
    result_actual <- ancova(dat, list(outcome = "out", group = "grp"))["trt"]

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
        "trt" = list(
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
        visit_level = "visit 1"
    )["trt"]

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
        "trt" = list(
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
        visit_level = "visit 1"
    )["trt"]

    expect_equal(result_expected, result_actual)
})


test_that("least_square_means", {

    set.seed(101)

    n <- 1000
    dat <- tibble(
        age1 = rnorm(n),
        age2 = rnorm(n),
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2,  sd = 20)
    )

    mod <- lm(out ~ age1 + age2 + grp, data = dat)

    mod2 <- ancova(
        dat,
        list(outcome = "out", group = "grp", covariates = c("age1", "age2"))
    )

    expect_equal(
        coef(mod)[["grpB"]],
        mod2$lsm_1$est - mod2$lsm_0$est
    )

    # x <- tibble(
    #     b = c(2, 4, 6),
    #     c = c(6, 8, 10)
    # )

    # sigma <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)

    # result_actual <- least_square_means(x,  ~ b + c - 1, c(1, 2), sigma)
    # result_expected <- list(
    #     est = sum(c(4, 8) * c(1, 2)),
    #     se = (c(4, 8) %*% sigma %*% c(4, 8)) %>%
    #         sqrt() %>%
    #         as.vector(),
    #     df = NA
    # )

    # expect_equal(result_actual, result_expected)

})




