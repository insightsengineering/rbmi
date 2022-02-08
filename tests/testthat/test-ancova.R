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

    vars <- set_vars(
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

    mu <- c(5, rep(0,3))

    sigma <- matrix(rep(0.6,16), ncol = 4)
    diag(sigma) <- 1

    d <- as.data.frame(t(replicate(n, sample_mvnorm(mu = mu, sigma = sigma), simplify = "matrix")))
    colnames(d) <- paste("x", 1:4, sep = "")

    d$x2 <- cut(d$x2,breaks=c(-Inf, -1, 0, 1, Inf))
    d$x3 <- cut(d$x3,breaks=c(-Inf, 0.5, Inf))
    d$x4 <- cut(d$x4,breaks=c(-Inf, -1, 0, 1, Inf))

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
