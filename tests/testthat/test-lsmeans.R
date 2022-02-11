library(dplyr)
library(testthat)

test_that("Least square means works as expected - Part 1", {

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





test_that("Least square means works as expected - Part 2", {
    n <- 8000

    dat <- tibble(
        trt = sample(c("C", "T"), size = n, replace = TRUE, prob = c(0.5, 0.5)),
        age = rnorm(n),
        sex = sample(c("M", "F", "O"), size = n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
        cat = sample(c("A", "B"), size = n, replace = TRUE, prob = c(0.8, 0.2)),
        visit = "vis1"
    ) %>%
        mutate(sex = factor(sex, levels = c("M", "F", "O"))) %>%
        mutate(trt = factor(trt, levels = c("C", "T"))) %>%
        mutate(cat = factor(cat, levels = c("A", "B")))


    outcome_vec <- (
        model.matrix(~ trt * sex * cat + age, dat) %*%
            c(10, 5, 4, 3, 9, 4, 2, 8, 1, -5, -1, -2, -3)
        ) + rnorm(n, 0, 5)

    dat2 <- dat %>%
        mutate(outcome = outcome_vec)


    mod <- lm(outcome ~ trt * sex * cat + age, data = dat2)


    ###########
    #
    # Proportional weighting
    #

    d <- suppressMessages({
        as.data.frame(emmeans::emmeans(mod, "trt", weights = "proportional"))
    })
    expected <- list(
        "est" = d[["emmean"]],
        "se" = d[["SE"]],
        "df" = d[["df"]]
    )
    lsm1 <- lsmeans(mod, trt = "C")
    lsm2 <- lsmeans(mod, trt = "T")
    actual <- list(
        "est" = c(lsm1$est, lsm2$est),
        "se" = c(lsm1$se, lsm2$se),
        "df" = c(lsm1$df, lsm2$df)
    )
    expect_equal(actual, expected)

    result_actual <- ancova(
        dat2,
        set_vars(
            visit = "visit",
            covariates = c("age", "sex*cat*trt"),
            outcome = "outcome",
            group = "trt"
        )
    )[c("lsm_ref_vis1", "lsm_alt_vis1")]
    result_expected <- list(
        "lsm_ref_vis1" = lsm1,
        "lsm_alt_vis1" = lsm2
    )
    expect_equal(result_actual, result_expected)


    ###########
    #
    # Equal weighting
    #

    d <- suppressMessages({
        as.data.frame(emmeans::emmeans(mod, "trt"))
    })
    expected <- list(
        "est" = d[["emmean"]],
        "se" = d[["SE"]],
        "df" = d[["df"]]
    )
    lsm1 <- lsmeans(mod, trt = "C", .weights = "equal")
    lsm2 <- lsmeans(mod, trt = "T", .weights = "equal")
    actual <- list(
        "est" = c(lsm1$est, lsm2$est),
        "se" = c(lsm1$se, lsm2$se),
        "df" = c(lsm1$df, lsm2$df)
    )
    expect_equal(actual, expected)

    result_actual <- ancova(
        dat2,
        set_vars(
            visit = "visit",
            covariates = c("age", "sex*cat*trt"),
            outcome = "outcome",
            group = "trt"
        ),
        weights = "equal"
    )[c("lsm_ref_vis1", "lsm_alt_vis1")]
    result_expected <- list(
        "lsm_ref_vis1" = lsm1,
        "lsm_alt_vis1" = lsm2
    )
    expect_equal(result_actual, result_expected)
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




#### Toy code to experiment with

# n <- 8000

# dat <- tibble(
#     trt = sample(c("C", "T"), size = n, replace = TRUE, prob = c(0.5, 0.5)),
#     age = rnorm(n),
#     sex = sample(c("M", "F", "O"), size = n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
#     cat = sample(c("A", "B"), size = n, replace = TRUE, prob = c(0.8, 0.2)),
#     visit = "vis1"
# ) %>%
#     mutate(sex = factor(sex, levels = c("M", "F", "O"))) %>%
#     mutate(trt = factor(trt, levels = c("C", "T"))) %>%
#     mutate(cat = factor(cat, levels = c("A", "B")))


# outcome_vec <- (
#     model.matrix(~ trt * sex * cat + age, dat) %*%
#         c(10, 5, 4, 3, 9, 4, 2, 8, 1, -5, -1, -2, -3)
#     ) + rnorm(n, 0, 5)

# dat2 <- dat %>%
#     mutate(outcome = outcome_vec)


# mod <- lm(outcome ~ trt * sex * cat + age, data = dat2)


# as.data.frame(emmeans::emmeans(mod, "trt", weights = "equal"))
# as.data.frame(emmeans::emmeans(mod, "trt", weights = "proportional"))

# dat_t <- dat2 %>% mutate(trt = factor("T", levels = c("C", "T")))
# dat_c <- dat2 %>% mutate(trt = factor("C", levels = c("C", "T")))
# mean(predict(mod, dat_c))
# mean(predict(mod, dat_t))










