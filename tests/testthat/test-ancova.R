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
    # set.seed(101)
    # n <- 20
    # dat <- tibble(
    #     c1 = rnorm(n),
    #     c2 = rnorm(n),
    #     grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),
    #     out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * c1 + 8 * c2,  sd = 4),
    #     vis = 1
    # )
    # dput(dat)
    dat <- structure(list(c1 = c( -0.326036490515386, 0.552461855419139,
        -0.67494384395583, 0.214359459043425, 0.310769217313602, 1.1739662875627,
        0.618789855625968, -0.112734314754215, 0.917028289512712, -0.223259364627263,
        0.526448098873642, -0.794844435415055, 1.42775554468304, -1.46681969416347,
        -0.236683378602896, -0.1933379649975, -0.849754740333862, 0.0584654978495017,
        -0.817670355875958, -2.05030781563963
    ), c2 = c( -0.163755665941423, 0.708522104156376, -0.267980546181617, -1.46392175987102, 0.744435822875318,
        -1.4103901810052, 0.467067606015246, -0.119320107694245, 0.467238961765515,
        0.498135564435914, 0.894937200540396, 0.279151996413743, 1.00786575031424,
        -2.07310649057379, 1.18985338378285, -0.724374218369974, 0.167983772348879,
        0.920335158580844, -1.67160481202292, 0.448469069614306
    ), grp = structure(c( 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L
    ), .Label = c("A", "B"), class = "factor"), out = c(
        45.7847875853616, 59.7801396076617, 50.4679842661684, 33.5584531708974, 63.1947943169161,
        39.2079969145174, 59.8732237531287, 56.1137383614101, 62.1009147091178,
        54.3805941295766, 58.2260815684598, 46.409146055023, 64.2085179649195,
        30.9976993353656, 63.2625108804055, 47.9789955579129, 45.1936579131275,
        61.1921532146968, 33.1258810863828, 48.0872278346664
    ), vis = c(1,1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), 
    row.names = c(NA, -20L), class = c("tbl_df", "tbl", "data.frame"))

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

    # dput(as.data.frame(emmeans::emmeans(mod, "grp")))
    x <- structure(list(grp = structure(1:2, .Label = c("A", "B"), class = "factor"), 
    emmean = c(48.4373706169725, 52.0424982927158), SE = c(1.44866393822928, 
    0.951401186394566), df = c(15, 15), lower.CL = c(45.3496165241649, 
    50.01463466633), upper.CL = c(51.5251247097802, 54.0703619191015
    )), class = "data.frame", row.names = 1:2, estName = "emmean", clNames = c("lower.CL", 
    "upper.CL"), pri.vars = "grp", adjust = "none", side = 0, delta = 0, type = "link", mesg = "Confidence level used: 0.95")


    result_expected <- list(
        list(est = x$emmean[[1]], se = x$SE[[1]], df = x$df[[1]]),
        list(est = x$emmean[[2]], se = x$SE[[2]], df = x$df[[2]])
    )

    result_actual <- list(
        lsmeans(mod, grp = "A"),
        lsmeans(mod, grp = "B")
    )

    expect_equal(result_actual, result_expected)

    expect_error(
        lsmeans(mod, group = "A"),
        regex = "`group`"
    )

})



