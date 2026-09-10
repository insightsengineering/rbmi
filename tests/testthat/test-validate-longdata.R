suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})

n <- 10
nv <- 3

covars <- tibble(
    subjid = 1:n,
    age = rnorm(n),
    group = factor(
        sample(c("A", "B"), size = n, replace = TRUE),
        levels = c("A", "B")
    ),
    sex = factor(
        sample(c("M", "F"), size = n, replace = TRUE),
        levels = c("M", "F")
    ),
    strata = c("A", "A", "A", "A", "A", "A", "A", "A", "B", "B")
)

dat <- tibble(
    subjid = rep.int(1:n, nv)
) %>%
    left_join(covars, by = "subjid") %>%
    mutate(
        outcome = rnorm(
            n(),
            age * 3 + (as.numeric(sex) - 1) * 3 + (as.numeric(group) - 1) * 4,
            sd = 3
        )
    ) %>%
    arrange(subjid) %>%
    group_by(subjid) %>%
    mutate(visit = factor(paste0("Visit ", seq_len(n())))) %>%
    ungroup() %>%
    mutate(subjid = factor(subjid))

dat[c(1, 2, 3, 4, 5, 7), "outcome"] <- NA


vars <- set_vars(
    outcome = "outcome",
    visit = "visit",
    subjid = "subjid",
    group = "group",
    strata = "strata",
    strategy = "strategy",
    covariates = c("sex", "age")
)

dat_period <- dat %>%
    mutate(
        period = as.character(as.integer(visit)),
        duration = rep(c(2, 0, 1), times = n)
    )

vars_period <- set_vars(
    outcome = "outcome",
    period = "period",
    duration = "duration",
    subjid = "subjid",
    group = "group",
    strata = "strata",
    strategy = "strategy",
    covariates = c("sex", "age")
)


test_that("period_levels orders periods according to their type", {
    period_data <- tibble(period = c(1, 10, 2))

    expect_equal(period_levels(period_data, vars_period), c("1", "2", "10"))

    period_data$period <- as.character(period_data$period)
    expect_equal(period_levels(period_data, vars_period), c("1", "10", "2"))

    period_data$period <- factor(
        period_data$period,
        levels = c("2", "10", "1")
    )
    expect_equal(period_levels(period_data, vars_period), c("2", "10", "1"))
})


test_that("extract_covariates", {
    expect_equal(extract_covariates("age"), "age")
    expect_equal(extract_covariates(c("age", "sex")), c("age", "sex"))
    expect_equal(extract_covariates(c("age:sex")), c("age", "sex"))
    expect_equal(extract_covariates(c("age*sex")), c("age", "sex"))
    expect_equal(extract_covariates(c("age", "age*sex")), c("age", "sex"))
    expect_equal(extract_covariates(c("age", " age*sex ")), c("age", "sex"))
    expect_equal(extract_covariates(c("age", " age *sex ")), c("age", "sex"))
    expect_equal(extract_covariates(c("age", " age * sex")), c("age", "sex"))
    expect_equal(extract_covariates(c("age", " age :   sex ")), c("age", "sex"))
    expect_equal(extract_covariates(""), character(0))
})


test_that("validate.ivars", {
    expect_true(validate(vars))
    expect_true(validate(vars_period))

    expect_error(
        set_vars(period = "period"),
        "`vars\\$duration`"
    )

    expect_error(
        set_vars(visit = "visit", period = "period", duration = "duration"),
        "Only one of `visit` and `period`"
    )

    vars2 <- vars
    vars2$subjid <- NULL
    expect_error(validate(vars2))

    vars2 <- vars
    vars2$outcome <- NULL
    expect_error(validate(vars2))

    vars2 <- vars
    vars2$group <- NULL
    expect_error(validate(vars2))

    vars2 <- vars
    vars2$visit <- NULL
    expect_error(validate(vars2))

    vars2 <- vars_period
    vars2$duration <- NULL
    expect_error(validate(vars2))

    vars2 <- vars_period
    vars2$duration <- c("duration", "duration2")
    expect_error(validate(vars2))

    vars2 <- vars
    vars2$strata <- 1
    expect_error(validate(vars2))

    vars2 <- vars
    vars2$strata <- NULL
    expect_true(validate(vars2))

    vars2 <- vars
    vars2$covariates <- 1
    expect_error(validate(vars2))

    vars2 <- vars
    vars2$covariates <- NULL
    expect_true(validate(vars2))

    vars2 <- vars
    vars2$subjid <- c("v1", "v2")
    expect_error(validate(vars2))
})


test_that("validate_datalong_varExists", {
    expect_true(validate_datalong_varExists(dat, vars))
    expect_true(validate_datalong_varExists(dat_period, vars_period))

    dat2 <- dat
    dat2$subjid <- NULL
    expect_error(validate_datalong_varExists(dat2, vars))

    dat2 <- dat
    dat2$age <- NULL
    expect_error(validate_datalong_varExists(dat2, vars))

    dat2 <- dat
    dat2$group <- NULL
    expect_error(validate_datalong_varExists(dat2, vars))

    dat2 <- dat
    dat2$sex <- NULL
    expect_error(validate_datalong_varExists(dat2, vars))

    dat2 <- dat
    dat2$strata <- NULL
    expect_error(validate_datalong_varExists(dat2, vars))

    dat2 <- dat
    dat2$outcome <- NULL
    expect_error(validate_datalong_varExists(dat2, vars))

    dat2 <- dat
    dat2$visit <- NULL
    expect_error(validate_datalong_varExists(dat2, vars))

    dat2 <- dat_period
    dat2$duration <- NULL
    expect_error(validate_datalong_varExists(dat2, vars_period))
})


test_that("validate_datalong_types", {
    expect_true(validate_datalong_types(dat, vars))
    expect_true(validate_datalong_types(dat_period, vars_period))

    dat2 <- dat
    dat2$subjid <- rnorm(nrow(dat))
    expect_error(validate_datalong_types(dat2, vars))

    dat2 <- dat
    dat2$subjid <- as.character(dat$subjid)
    expect_error(validate_datalong_types(dat2, vars))

    dat2 <- dat
    dat2$subjid <- factor(dat$subjid)
    expect_true(validate_datalong_types(dat2, vars))

    dat2 <- dat
    dat2$group <- rnorm(nrow(dat))
    expect_error(validate_datalong_types(dat2, vars))

    dat2 <- dat
    dat2$group <- factor(dat$subjid)
    expect_true(validate_datalong_types(dat2, vars))

    dat2 <- dat
    dat2$outcome <- as.character(dat$outcome)
    expect_error(validate_datalong_types(dat2, vars))

    dat2 <- dat
    dat2$outcome <- as.factor(dat$outcome)
    expect_error(validate_datalong_types(dat2, vars))

    dat2 <- dat
    dat2$visit <- rnorm(nrow(dat))
    expect_error(validate_datalong_types(dat2, vars))

    dat2 <- dat
    dat2$visit <- as.character(dat$visit)
    expect_error(validate_datalong_types(dat2, vars))

    # Show that we can catch covariates that only have 1 level
    dat2 <- dat
    dat2$sex <- factor("M")
    expect_error(validate_datalong_types(dat2, vars), "`sex`")

    # Show that covariate 1 level checks only apply to covariates
    dat2 <- dat
    dat2$group <- factor("A")
    expect_true(validate_datalong_types(dat2, vars))

    # But then show that it will catch group if it is listed as a covariate
    dat2 <- dat
    dat2$group <- factor("A")
    vars2 <- vars
    vars2$covariates <- c(vars2$covariates, "group*sex")
    expect_error(validate_datalong_types(dat2, vars2), "`group`")

    # Show that stratification variables are not affected by this
    dat2 <- dat
    dat2$strata <- factor("A")
    expect_true(validate_datalong_types(dat2, vars))

    # Show that the visit variable is not affected by this
    dat2 <- dat
    dat2$visit <- factor("A")
    expect_true(validate_datalong_types(dat2, vars))

    # The period can be character, factor, or numeric, with arbitrary values.
    dat2 <- dat_period
    dat2$period <- factor(dat2$period)
    expect_true(validate_datalong_types(dat2, vars_period))
    dat2$period <- as.numeric(as.character(dat2$period))
    expect_true(validate_datalong_types(dat2, vars_period))

    # Test that if group or visit variables have unobserved levels return error
    dat2 <- dat[dat$visit != "Visit 1", ]
    expect_error(
        validate_datalong_types(dat2, vars),
        "`visit`"
    )
    dat2 <- dat[dat$group == "B", ]
    expect_error(
        validate_datalong_types(dat2, vars),
        "`group`"
    )

    dat2 <- dat_period
    dat2$period[1] <- "4"
    expect_true(validate_datalong_types(dat2, vars_period))

    dat2 <- dat_period
    dat2$duration[1] <- -1
    expect_error(validate_datalong_types(dat2, vars_period), "`duration`")

    dat2 <- dat_period
    dat2$duration[1] <- Inf
    expect_error(validate_datalong_types(dat2, vars_period), "`duration`")

    dat2 <- dat_period
    dat2$duration <- as.character(dat2$duration)
    expect_error(validate_datalong_types(dat2, vars_period), "`duration`")
})


test_that("validate_datalong_notMissing", {
    expect_true(validate_datalong_notMissing(dat, vars))
    expect_true(validate_datalong_notMissing(dat_period, vars_period))

    dat2 <- dat
    dat2$age[c(1, 2, 3)] <- NA
    expect_error(validate_datalong_notMissing(dat2, vars))

    dat2 <- dat
    dat2$group[c(1, 2, 3)] <- NA
    expect_error(validate_datalong_notMissing(dat2, vars))

    dat2 <- dat
    dat2$visit[c(1, 2, 3)] <- NA
    expect_error(validate_datalong_notMissing(dat2, vars))

    dat2 <- dat
    dat2$subjid[c(1, 2, 3)] <- NA
    expect_error(validate_datalong_notMissing(dat2, vars))

    dat2 <- dat
    dat2$sex[c(1, 2, 3)] <- NA
    expect_error(validate_datalong_notMissing(dat2, vars))

    dat2 <- dat
    dat2$strata[c(1, 2, 3)] <- NA
    expect_error(validate_datalong_notMissing(dat2, vars))

    dat2 <- dat_period
    dat2$duration[c(1, 2, 3)] <- NA
    expect_error(
        validate_datalong_notMissing(dat2, vars_period),
        "Variable duration contains missing data"
    )

    dat2 <- dat_period
    dat2$period[c(1, 2, 3)] <- NA
    expect_error(validate_datalong_notMissing(dat2, vars_period))
})


test_that("validate_datalong_complete", {
    expect_true(validate_datalong_complete(dat, vars))
    expect_true(validate_datalong_complete(dat_period, vars_period))

    ### Duplicate visits per patient
    dat2 <- bind_rows(dat, dat)
    expect_error(validate_datalong_complete(dat2, vars))

    dat2 <- dat
    dat2 <- dat2[-1, ]
    expect_error(validate_datalong_complete(dat2, vars))

    ### Completely remove 1 visit (should check against the levels)
    dat2 <- dat %>% filter(visit != "Visit 1")
    expect_error(validate_datalong_complete(dat2, vars))

    dat2 <- dat_period %>% filter(period != "3")
    expect_true(validate_datalong_complete(dat2, vars_period))

    dat2 <- dat_period[-1, ]
    expect_true(validate_datalong_complete(dat2, vars_period))
    expect_s3_class(longDataConstructor$new(dat2, vars_period), "longdata")

    dat2 <- bind_rows(dat_period, dat_period[1, ])
    expect_error(validate_datalong_complete(dat2, vars_period), "duplicated")
})


test_that("validate_datalong_uniformStrata", {
    expect_true(validate_datalong_uniformStrata(dat, vars))

    vars2 <- vars
    vars2$strata <- character(0)
    expect_true(validate_datalong_uniformStrata(dat, vars2))

    dat2 <- dat
    dat2$strata[[1]] <- "AXS"
    expect_error(validate_datalong_uniformStrata(dat2, vars))
})


test_that("validate_data_long", {
    expect_true(validate_datalong(dat, vars))
    expect_true(validate_datalong(dat_period, vars_period))

    dat2 <- dat_period
    dat2$duration[1] <- NA_real_
    expect_error(
        validate_datalong(dat2, vars_period),
        "Variable duration contains missing data"
    )
})


test_that("validate_data_ice", {
    di <- data.frame(
        subjid = c("1", "1"),
        strategy = c("MAR", "MAR"),
        visit = c("Visit 1"),
        stringsAsFactors = FALSE
    )

    expect_error(
        validate_dataice(dat, di, vars),
        regexp = "must contain at most 1 row per"
    )

    di <- data.frame(
        subjid = c("1", "2"),
        strategy = c("MAR", "MAR"),
        visit = c("Visit 20"),
        stringsAsFactors = FALSE
    )

    expect_error(
        validate_dataice(dat, di, vars),
        regexp = "vars\\$visit.*contains values that are"
    )

    di <- data.frame(
        subjid = c("1", "2"),
        strategy = c("MAR", NA),
        visit = c("Visit 1"),
        stringsAsFactors = FALSE
    )

    expect_error(
        validate_dataice(dat, di, vars),
        regexp = "vars\\$strategy.* must be a non"
    )

    di <- data.frame(
        subjid = c("1", "abc"),
        strategy = c("MAR", "CR"),
        visit = c("Visit 1"),
        stringsAsFactors = FALSE
    )

    expect_error(
        validate_dataice(dat, di, vars),
        regexp = "vars\\$subjid.* contains values that aren't"
    )

    di <- data.frame(
        subjid = c("1", "2"),
        strategy = c("MAR", "CR"),
        visit = c("Visit 1"),
        stringsAsFactors = FALSE
    )

    expect_true(validate_dataice(dat, di, vars))

    di <- data.frame(
        subjid = c("1", "2"),
        strategy = c("MAR", "CR"),
        visit = c("Visit 122"),
        stringsAsFactors = FALSE
    )

    expect_true(validate_dataice(dat, di, vars, update = TRUE))
})
