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
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2, sd = 20)
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
        list(
            outcome = "out",
            group = "grp",
            covariates = c("age1", "age2"),
            visit = "visit"
        )
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
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2, sd = 20)
    )

    mod <- lm(out ~ grp, data = dat)

    result_expected <- list(
        "trt_ 1" = list(
            "est" = mod$coefficients[[2]],
            "se" = sqrt(vcov(mod)[2, 2]),
            "df" = df.residual(mod)
        )
    )
    result_actual <- ancova(
        dat,
        list(outcome = "out", group = "grp", visit = "ivis")
    )["trt_ 1"]

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
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2, sd = 20)
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
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2, sd = 20)
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
            "trt_visit 1",
            "lsm_ref_visit 1",
            "lsm_alt_visit 1",
            "trt_visit 2",
            "lsm_ref_visit 2",
            "lsm_alt_visit 2"
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
        out = rnorm(n, mean = 50 + 3 * f2n(grp) + 4 * age1 + 8 * age2, sd = 20)
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


test_that("ancova_m_groups - basic 3-group functionality", {
    set.seed(201)

    n <- 900
    dat <- tibble(
        visit = "vis1",
        age = rnorm(n),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE)),
        grp = factor(sample(c("Control", "Trt1", "Trt2"), size = n, replace = TRUE)),
        out = rnorm(n, mean = 50 + 2 * age + 3 * f2n(sex) +
                        4 * (grp == "Trt1") + 6 * (grp == "Trt2"), sd = 10)
    )

    # Manual model for comparison
    mod <- lm(out ~ age + sex + grp, data = dat)

    # Test ancova_m_groups
    result_actual <- ancova_m_groups(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = c("age", "sex"),
            visit = "visit"
        )
    )

    # Check output structure
    expected_names <- c(
        "trt_L1_L0_vis1", "trt_L2_L0_vis1",  # Treatment effects vs reference
        "lsm_L0_vis1", "lsm_L1_vis1", "lsm_L2_vis1"  # LSMeans for all groups
    )
    expect_equal(sort(names(result_actual)), sort(expected_names))


    expected_trt_L1_L0_vis1 <- list(
        est = coef(mod)[["grpTrt1"]],
        se = sqrt(vcov(mod)["grpTrt1", "grpTrt1"]),
        df = df.residual(mod)
    )
    expect_equal(result_actual$trt_L1_L0_vis1, expected_trt_L1_L0_vis1)


    expected_trt_L2_L0_vis1 <- list(
        est = coef(mod)[["grpTrt2"]],
        se = sqrt(vcov(mod)["grpTrt2", "grpTrt2"]),
        df = df.residual(mod)
    )
    expect_equal(result_actual$trt_L2_L0_vis1, expected_trt_L2_L0_vis1)
})


test_that("ancova_m_groups - 4-group functionality", {
    set.seed(301)

    n <- 800
    grp_levels <- c("Placebo", "Low", "Medium", "High")
    dat <- tibble(
        visit = "baseline",
        baseline_score = rnorm(n, mean = 30, sd = 8),
        grp = factor(sample(grp_levels, size = n, replace = TRUE), levels = grp_levels),
        out = rnorm(n, mean = 20 + 0.7 * baseline_score +
                        2 * (grp == "Low") +
                        5 * (grp == "Medium") +
                        8 * (grp == "High"), sd = 6)
    )

    # Manual model for comparison
    mod <- lm(out ~ baseline_score + grp, data = dat)

    result_actual <- ancova_m_groups(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = "baseline_score",
            visit = "visit"
        )
    )

    # Check we have all expected components
    expected_names <- c(
        "trt_L1_L0_baseline", "trt_L2_L0_baseline", "trt_L3_L0_baseline",  # 3 treatment effects
        "lsm_L0_baseline", "lsm_L1_baseline", "lsm_L2_baseline", "lsm_L3_baseline"  # 4 LSMeans
    )
    expect_equal(sort(names(result_actual)), sort(expected_names))

    # Verify treatment effects
    expect_equal(result_actual$trt_L1_L0_baseline$est, coef(mod)[["grpLow"]], tolerance = 1e-10)
    expect_equal(result_actual$trt_L2_L0_baseline$est, coef(mod)[["grpMedium"]], tolerance = 1e-10)
    expect_equal(result_actual$trt_L3_L0_baseline$est, coef(mod)[["grpHigh"]], tolerance = 1e-10)
})


test_that("ancova_m_groups - LSMeans with group interactions", {
    set.seed(401)

    grp_levels <- c("Control", "Treatment1", "Treatment2")
    sex_levels <- c("Male", "Female")
    n <- 600
    dat <- tibble(
        visit = "week12",
        age = rnorm(n, mean = 45, sd = 12),
        sex = factor(sample(sex_levels, size = n, replace = TRUE), levels = sex_levels),
        grp = factor(sample(grp_levels, size = n, replace = TRUE), levels = grp_levels),
        # Create interaction effect between group and sex
        out = rnorm(n, mean = 40 + 0.5 * age +
                        3 * (sex == "Female") +
                        5 * (grp == "Treatment1") +
                        7 * (grp == "Treatment2") +
                        # Group-sex interactions
                        2 * (grp == "Treatment1" & sex == "Female") +
                        -1 * (grp == "Treatment2" & sex == "Female"),
                    sd = 8)
    )

    # Test with group*sex interaction in covariates
    result_actual <- ancova_m_groups(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = c("age", "sex", "grp*sex"),  # Note: using rbmiGroup for interaction
            visit = "visit"
        )
    )

    mod <- lm(out ~ age + sex + grp + grp:sex, data = dat)


    # Check structure
    expected_names <- c(
        "trt_L1_L0_week12", "trt_L2_L0_week12",
        "lsm_L0_week12", "lsm_L1_week12", "lsm_L2_week12"
    )
    expect_equal(sort(names(result_actual)), sort(expected_names))


    suppressMessages({
        emmean_counter <- as.data.frame(
            emmeans::emmeans(mod, "grp", counterfactual = "grp")
        )
    })

    expected <- as.list(emmean_counter[emmean_counter$grp == "Control", c("emmean", "SE", "df")])
    names(expected) <- c("est", "se", "df")
    expect_equal(result_actual$lsm_L0_week12, expected)

    expected <- as.list(emmean_counter[emmean_counter$grp == "Treatment1", c("emmean", "SE", "df")])
    names(expected) <- c("est", "se", "df")
    expect_equal(result_actual$lsm_L1_week12, expected)

    expected <- as.list(emmean_counter[emmean_counter$grp == "Treatment2", c("emmean", "SE", "df")])
    names(expected) <- c("est", "se", "df")
    expect_equal(result_actual$lsm_L2_week12, expected)


    # Same again but with a different weighting scheme
    result_actual <- ancova_m_groups(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = c("age", "sex", "grp*sex"), # Note: using rbmiGroup for interaction
            visit = "visit"
        ),
        weights = "equal"
    )
    suppressMessages({
        emmean_counter <- as.data.frame(
            emmeans::emmeans(mod, "grp", "grp")
        )
    })
    expected <- as.list(emmean_counter[emmean_counter$grp == "Control", c("emmean", "SE", "df")])
    names(expected) <- c("est", "se", "df")
    expect_equal(result_actual$lsm_L0_week12, expected)

    expected <- as.list(emmean_counter[emmean_counter$grp == "Treatment1", c("emmean", "SE", "df")])
    names(expected) <- c("est", "se", "df")
    expect_equal(result_actual$lsm_L1_week12, expected)

    expected <- as.list(emmean_counter[emmean_counter$grp == "Treatment2", c("emmean", "SE", "df")])
    names(expected) <- c("est", "se", "df")
    expect_equal(result_actual$lsm_L2_week12, expected)

})


test_that("ancova_m_groups - multiple visits", {
    set.seed(601)

    n_per_visit <- 200
    grp_levels <- c("A", "B", "C")
    dat <- bind_rows(
        tibble(
            visit = "visit1",
            age = rnorm(n_per_visit),
            grp = factor(sample(grp_levels, size = n_per_visit, replace = TRUE), levels = grp_levels),
            out = rnorm(n_per_visit, mean = 20 + 2 * age +
                            3 * (grp == "B") + 5 * (grp == "C"), sd = 4)
        ),
        tibble(
            visit = "visit2",
            age = rnorm(n_per_visit),
            grp = factor(sample(c("A", "B", "C"), size = n_per_visit, replace = TRUE)),
            out = rnorm(n_per_visit, mean = 25 + 2 * age +
                            4 * (grp == "B") + 7 * (grp == "C"), sd = 4)
        )
    )

    result_actual <- ancova_m_groups(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = "age",
            visit = "visit"
        )
    )

    # Check we have results for both visits
    visit1_names <- grep("_visit1$", names(result_actual), value = TRUE)
    visit2_names <- grep("_visit2$", names(result_actual), value = TRUE)

    expect_length(visit1_names, 5)  # 2 treatment effects + 3 LSMeans
    expect_length(visit2_names, 5)  # 2 treatment effects + 3 LSMeans

    expected_names <- c(
        "trt_L1_L0_visit1", "trt_L2_L0_visit1", "lsm_L0_visit1", "lsm_L1_visit1", "lsm_L2_visit1",
        "trt_L1_L0_visit2", "trt_L2_L0_visit2",  "lsm_L0_visit2", "lsm_L1_visit2", "lsm_L2_visit2"
    )
    expect_true(all(expected_names %in% names(result_actual)))
})


test_that("ancova_m_groups - no covariates", {
    set.seed(701)

    n <- 300
    grp_levels <- c("Control", "Low", "High")
    dat <- tibble(
        visit = "final",
        grp = factor(sample(grp_levels, size = n, replace = TRUE), levels = grp_levels),
        out = rnorm(n, mean = 40 + 3 * (grp == "Low") + 8 * (grp == "High"), sd = 6)
    )

    result_actual <- ancova_m_groups(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = NULL,
            visit = "visit"
        )
    )

    # Manual model for comparison
    mod <- lm(out ~ grp, data = dat)

    # Check treatment effects
    expect_equal(
        result_actual$trt_L1_L0_final$est,
        coef(mod)[["grpLow"]],
        tolerance = 1e-10
    )
    expect_equal(
        result_actual$trt_L2_L0_final$est,
        coef(mod)[["grpHigh"]],
        tolerance = 1e-10
    )

    # Check we have all expected components
    expected_names <- c(
        "trt_L1_L0_final", "trt_L2_L0_final",
        "lsm_L0_final", "lsm_L1_final", "lsm_L2_final"
    )
    expect_equal(sort(names(result_actual)), sort(expected_names))
})


test_that("ancova_m_groups - error handling", {
    set.seed(801)

    n <- 100
    dat <- tibble(
        visit = "v1",
        age = rnorm(n),
        grp = factor(sample(c("A", "B"), size = n, replace = TRUE)),  # Only 2 groups
        out = rnorm(n)
    )

    # Should work with 2 groups (minimum requirement)
    expect_no_error({
        result <- ancova_m_groups(
            dat,
            list(outcome = "out", group = "grp", covariates = "age", visit = "visit")
        )
    })

    # Test with single group - should fail
    dat_single_group <- dat
    dat_single_group$grp <- factor(rep("A", n))

    expect_error(
        ancova_m_groups(
            dat_single_group,
            list(outcome = "out", group = "grp", covariates = "age", visit = "visit")
        ),
        regexp = "must be a factor variable with >=2 levels"
    )

    # Test reserved variable name conflict
    dat_conflict <- dat
    dat_conflict$rbmiGroup <- 1

    expect_error(
        ancova_m_groups(
            dat_conflict,
            list(outcome = "out", group = "grp", covariates = c("age", "rbmiGroup"), visit = "visit")
        ),
        regexp = "rbmiGroup.*reserved variable name"
    )
})


test_that("ancova_m_groups - backward compatibility with 2-group case", {
    set.seed(901)

    n <- 400
    grp_levels <- c("Control", "Treatment")
    sex_levels <- c("M", "F")
    dat <- tibble(
        visit = "week8",
        age = rnorm(n),
        sex = factor(sample(sex_levels, size = n, replace = TRUE), levels = sex_levels),
        grp = factor(sample(grp_levels, size = n, replace = TRUE), levels = grp_levels),
        out = rnorm(n, mean = 30 + 2 * age + 3 * f2n(sex) + 5 * f2n(grp), sd = 7)
    )

    # Compare ancova_m_groups with traditional ancova for 2-group case
    result_traditional <- ancova(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = c("age", "sex"),
            visit = "visit"
        )
    )

    result_multigroup <- ancova_m_groups(
        dat,
        list(
            outcome = "out",
            group = "grp",
            covariates = c("age", "sex"),
            visit = "visit"
        )
    )

    expect_equal(
        result_traditional$trt_week8,
        result_multigroup$trt_L1_L0_week8,
        tolerance = 1e-10
    )

    expect_equal(
        result_traditional$lsm_ref_week8,
        result_multigroup$lsm_L0_week8,
        tolerance = 1e-10
    )

    expect_equal(
        result_traditional$lsm_alt_week8,
        result_multigroup$lsm_L1_week8,
        tolerance = 1e-10
    )

    expect_true(length(result_traditional) == length(result_multigroup))
})
