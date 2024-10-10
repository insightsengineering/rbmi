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
    group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
    sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F")),
    strata = c("A", "A", "A", "A", "A", "A", "A", "A", "B", "B")
)

dat <- tibble(
    subjid = rep.int(1:n, nv)
) %>%
    left_join(covars, by = "subjid") %>%
    mutate(outcome = rnorm(
        n(),
        age * 3 + (as.numeric(sex) - 1) * 3 + (as.numeric(group) - 1) * 4,
        sd = 3
    )) %>%
    arrange(subjid) %>%
    group_by(subjid) %>%
    mutate(visit = factor(paste0("Visit ", seq_len(n()))))  %>%
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

})




test_that("validate_datalong_types", {


    expect_true(validate_datalong_types(dat, vars))


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
})


test_that("validate_datalong_notMissing", {

    expect_true(validate_datalong_notMissing(dat, vars))

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

})



test_that("validate_datalong_complete", {

    expect_true(validate_datalong_complete(dat, vars))

    ### Duplicate visits per patient
    dat2 <- bind_rows(dat, dat)
    expect_error(validate_datalong_complete(dat2, vars))

    dat2 <- dat
    dat2 <- dat2[-1, ]
    expect_error(validate_datalong_complete(dat2, vars))

    ### Completely remove 1 visit (should check against the levels)
    dat2 <- dat %>% filter(visit != "Visit 1")
    expect_error(validate_datalong_complete(dat2, vars))
})


test_that("validate_datalong_unifromStrata", {

    expect_true(validate_datalong_unifromStrata(dat, vars))

    vars2 <- vars
    vars2$strata <- character(0)
    expect_true(validate_datalong_unifromStrata(dat, vars2))

    dat2 <- dat
    dat2$strata[[1]] <- "AXS"
    expect_error(validate_datalong_unifromStrata(dat2, vars))

})


test_that("validate_data_long", {
    expect_true(validate_datalong(dat, vars))
})


test_that("validate_data_ice", {

    di <- data.frame(
        subjid = c("1", "1"),
        strategy = c("MAR", "MAR"),
        visit = c("Visit 1"),
        stringsAsFactors = FALSE
    )

    expect_error(validate_dataice(dat, di, vars), regexp = "must contain at most 1 row per")

    di <- data.frame(
        subjid = c("1", "2"),
        strategy = c("MAR", "MAR"),
        visit = c("Visit 20"),
        stringsAsFactors = FALSE
    )

    expect_error(validate_dataice(dat, di, vars),  regexp = "vars\\$visit.*contains values that are")

    di <- data.frame(
        subjid = c("1", "2"),
        strategy = c("MAR", NA),
        visit = c("Visit 1"),
        stringsAsFactors = FALSE
    )

    expect_error(validate_dataice(dat, di, vars),  regexp = "vars\\$strategy.* must be a non")

    di <- data.frame(
        subjid = c("1", "abc"),
        strategy = c("MAR", "CR"),
        visit = c("Visit 1"),
        stringsAsFactors = FALSE
    )

    expect_error(validate_dataice(dat, di, vars),  regexp = "vars\\$subjid.* contains values that aren't")

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
