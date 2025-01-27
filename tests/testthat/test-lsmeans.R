library(dplyr)
library(testthat)

test_that("Least square means works as expected", {

    ##### Check that treatment effect is unaffected
    set.seed(101)
    n <- 20
    dat <- tibble(
        c1 = rnorm(n),
        c2 = rnorm(n),
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * c1 + 8 * c2,  sd = 4),
        vis = 1
    )

    mod <- lm(out ~ c1 + c2 + grp + c1 * c2, data = dat)

    mod2 <- ancova(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = c("c1", "c2", "c1 * c2"),
            visit = "vis"
        )
    )

    expect_equal(
        coef(mod)[["grpB"]],
        mod2$lsm_alt_1$est - mod2$lsm_ref_1$est
    )


    ####### Comparing directly to emmeans (pre-computed values)

    x <- as.data.frame(emmeans::emmeans(mod, "grp"))

    result_expected <- list(
        list(est = x$emmean[[1]], se = x$SE[[1]], df = x$df[[1]]),
        list(est = x$emmean[[2]], se = x$SE[[2]], df = x$df[[2]])
    )

    result_actual <- list(
        lsmeans(mod, grp = "A", .weights = "equal"),
        lsmeans(mod, grp = "B", .weights = "equal")
    )

    expect_equal(result_actual, result_expected)

    expect_error(
        lsmeans(mod, group = "A"),
        regex = "`group`"
    )

})


test_that("LSmeans (proportional) from rbmi equals means of average prediction", {
    set.seed(124)

    # simulate data
    n <- 40

    mu <- c(5, rep(0, 3))

    sigma <- matrix(rep(0.6, 16), ncol = 4)
    diag(sigma) <- 1

    d <- as.data.frame(
        t(
            replicate(
                n,
                sample_mvnorm(mu = mu, sigma = sigma),
                simplify = "matrix"
            )
        )
    )
    colnames(d) <- paste("x", 1:4, sep = "")

    d$x2 <- cut(d$x2, breaks = c(-Inf, -1, 0, 1, Inf))
    d$x3 <- cut(d$x3, breaks = c(-Inf, 0.5, Inf))
    d$x4 <- cut(d$x4, breaks = c(-Inf, -1, 0, 1, Inf))

    d$trt <- factor(rep(c("Control", "Intervention"), c(10, n - 10)))

    d$y <- 1 * d$x1 + 2 * (d$x2 == "(-1,0]") + 3 * (d$trt == "Intervention") + rnorm(n)

    # LSmeans by hand (predict -> average)
    fit <- lm(y ~ trt + x1 + x2 + x3 + x4, data = d)

    d_ctrl <- d
    d_ctrl$trt <- "Control"
    lsmeans_ctrl <- mean(predict(fit, newdata = d_ctrl))

    d_interv <- d
    d_interv$trt <- "Intervention"
    lsmeans_interv <- mean(predict(fit, newdata = d_interv))


    # LSmeans using rbmi (average -> predict)

    lsmeans_ctrl2 <- lsmeans(fit, trt = "Control")$est
    lsmeans_interv2 <- lsmeans(fit, trt = "Intervention")$est

    expect_equal(lsmeans_ctrl2, lsmeans_ctrl)
    expect_equal(lsmeans_interv2, lsmeans_interv)
})


test_that("LSmeans(proportional) returns equivalent results to 'counterfactual'", {
    set.seed(2412)
    n <- 4000
    dat <- tibble(
        v1 = rnorm(n),
        v2 = rnorm(n),
        v3 = rnorm(n),
        c1 = sample(c("A", "B"), size = n, replace = TRUE, prob = c(0.8, 0.2)),
        c2 = sample(c("Y", "X"), size = n, replace = TRUE, prob = c(0.6, 0.4)),
        c3 = sample(c("L", "K", "J"), size = n, replace = TRUE, prob = c(0.2, 0.5, 0.3)),
        error = rnorm(n, 0, 4),
        outcome = 30 +
            5 * v1 +
            3 * v2 +
            2 * v3 +
            8 * v1 * v2 +
            9 * v1 * v3 +
            10 * v2 * v3 +
            12 * v1 * v2 * v3 +
            4 * (c1 == "B") +
            6 * (c2 == "Y") +
            7 * (c1 == "B" & c2 == "Y") +
            13 * (c1 == "B") * v1 +
            15 * (c3 == "K") +
            16 * (c3 == "J") +
            error
    )
    dat$c2 <- factor(dat$c2, levels = c("Y", "X"))
    dat$c3 <- factor(dat$c3, levels = c("L", "K", "J"))
    mod <- lm(outcome ~ (v1 * v2 * v3) + (c1 * c2) + (v1 * c1) + c3, data = dat)


    #
    #
    # Equal
    #
    #
    emod <- suppressMessages({
        as.data.frame(emmeans::emmeans(mod, "c1", weights = "equal"))
    })
    expected <- list(
        "est" = emod[["emmean"]],
        "se" = emod[["SE"]],
        "df" = emod[["df"]]
    )
    lsm1 <- lsmeans(mod, c1 = "A", .weights = "equal")
    lsm2 <- lsmeans(mod, c1 = "B", .weights = "equal")
    actual <- list(
        "est" = c(lsm1$est, lsm2$est),
        "se" = c(lsm1$se, lsm2$se),
        "df" = c(lsm1$df, lsm2$df)
    )
    expect_equal(actual, expected)


    #
    #
    # Proportional
    #
    #
    emod <- suppressMessages({
        as.data.frame(emmeans::emmeans(mod, "c1", weights = "proportional"))
    })
    expected <- list(
        "est" = emod[["emmean"]],
        "se" = emod[["SE"]],
        "df" = emod[["df"]]
    )
    lsm1 <- lsmeans(mod, c1 = "A", .weights = "proportional_em")
    lsm2 <- lsmeans(mod, c1 = "B", .weights = "proportional_em")
    actual <- list(
        "est" = c(lsm1$est, lsm2$est),
        "se" = c(lsm1$se, lsm2$se),
        "df" = c(lsm1$df, lsm2$df)
    )
    expect_equal(actual, expected)


    #
    #
    # Counterfactual
    #
    #
    emod <- suppressMessages({
        as.data.frame(emmeans::emmeans(mod, "c1", counterfactual = "c1"))
    })
    expected <- list(
        "est" = emod[["emmean"]],
        "se" = emod[["SE"]],
        "df" = emod[["df"]]
    )
    lsm1 <- lsmeans(mod, c1 = "A", .weights = "counterfactual")
    lsm2 <- lsmeans(mod, c1 = "B", .weights = "counterfactual")
    actual <- list(
        "est" = c(lsm1$est, lsm2$est),
        "se" = c(lsm1$se, lsm2$se),
        "df" = c(lsm1$df, lsm2$df)
    )
    expect_equal(actual, expected)

    expect_message(
        lsmeans(mod, c1 = "A", .weights = "proportional"),
        "NOTE: The `proportional` weighting scheme is an alias for `counterfactual`"
    )

})


test_that("lsmeans correctly handles case when only using 1 categorical (#412)", {

    set.seed(2412)
    n <- 4000
    dat <- tibble(
        v1 = rnorm(n),
        v2 = rnorm(n),
        v3 = rnorm(n),
        c1 = sample(c("A", "B"), size = n, replace = TRUE, prob = c(0.8, 0.2)),
        error = rnorm(n, 0, 4),
        outcome = 30 +
            5 * v1 +
            3 * v2 +
            2 * v3 +
            8 * v1 * v2 +
            9 * v1 * v3 +
            10 * v2 * v3 +
            12 * v1 * v2 * v3 +
            4 * (c1 == "B") +
            13 * (c1 == "B") * v1 +
            error
    )
    mod <- lm(outcome ~ (v1 * v2 * v3) + (c1), data = dat)


    #
    #
    # Equal
    #
    #
    emod <- suppressMessages({
        as.data.frame(emmeans::emmeans(mod, "c1", weights = "equal"))
    })
    expected <- list(
        "est" = emod[["emmean"]],
        "se" = emod[["SE"]],
        "df" = emod[["df"]]
    )
    lsm1 <- lsmeans(mod, c1 = "A", .weights = "equal")
    lsm2 <- lsmeans(mod, c1 = "B", .weights = "equal")
    actual <- list(
        "est" = c(lsm1$est, lsm2$est),
        "se" = c(lsm1$se, lsm2$se),
        "df" = c(lsm1$df, lsm2$df)
    )
    expect_equal(actual, expected)


    #
    #
    # Proportional
    #
    #
    emod <- suppressMessages({
        as.data.frame(emmeans::emmeans(mod, "c1", weights = "proportional"))
    })
    expected <- list(
        "est" = emod[["emmean"]],
        "se" = emod[["SE"]],
        "df" = emod[["df"]]
    )
    lsm1 <- lsmeans(mod, c1 = "A", .weights = "proportional_em")
    lsm2 <- lsmeans(mod, c1 = "B", .weights = "proportional_em")
    actual <- list(
        "est" = c(lsm1$est, lsm2$est),
        "se" = c(lsm1$se, lsm2$se),
        "df" = c(lsm1$df, lsm2$df)
    )
    expect_equal(actual, expected)


    #
    #
    # Counterfactual
    #
    #
    emod <- suppressMessages({
        as.data.frame(emmeans::emmeans(mod, "c1", counterfactual = "c1"))
    })
    expected <- list(
        "est" = emod[["emmean"]],
        "se" = emod[["SE"]],
        "df" = emod[["df"]]
    )
    lsm1 <- lsmeans(mod, c1 = "A", .weights = "counterfactual")
    lsm2 <- lsmeans(mod, c1 = "B", .weights = "counterfactual")
    actual <- list(
        "est" = c(lsm1$est, lsm2$est),
        "se" = c(lsm1$se, lsm2$se),
        "df" = c(lsm1$df, lsm2$df)
    )
    expect_equal(actual, expected)
})

