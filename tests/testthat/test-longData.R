
suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})


ld_2_list <- function(ld) {

    vars <- c(
        "visits",
        "is_mar",
        "data",
        "ids",
        "group",
        "indexes",
        "vars",
        "strata",
        "strategies",
        "strategy_lock",
        "values",
        "ice_visit_index",
        "is_missing",
        "is_post_ice"
    )

    assert_that(
        all(vars %in% names(ld))
    )

    HOLD <- lapply(vars, function(x, ld) ld[[x]], ld = ld)
    names(HOLD) <- vars
    return(HOLD)
}



get_ld <- function() {
    n <- 4
    nv <- 3

    covars <- tibble(
        subjid = 1:n,
        age = rnorm(n),
        group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F")),
        strata = c("A", "A", "A", "B")
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

    dat[c(1, 2, 3, 4, 6, 7), "outcome"] <- NA


    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "subjid",
        group = "group",
        strata = "strata",
        covariates = c("sex", "age"),
        strategy = "strategy"
    )

    ld <- longDataConstructor$new(
        data = dat,
        vars = vars
    )

    return(list(ld = ld, dat = dat, n = n, nv = nv))
}


get_data <- function(n) {
    sigma <- as_vcov(c(2, 1, 0.7), c(0.5, 0.3, 0.2))

    set.seed(1518)

    dat <- get_sim_data(n, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss) %>%
        mutate(group = factor(group, labels = c("Placebo", "TRT")))


    dat_ice <- dat %>%
        group_by(id) %>%
        arrange(id, visit) %>%
        filter(is.na(outcome)) %>%
        slice(1) %>%
        ungroup() %>%
        select(id, visit) %>%
        mutate(strategy = "JR")


    vars <- set_vars(
        outcome = "outcome",
        group = "group",
        strategy = "strategy",
        subjid = "id",
        visit = "visit",
        covariates = c("age", "sex", "visit * group")
    )
    list(dat = dat, dat_ice = dat_ice, vars = vars)
}



test_that("longData - Basics", {

    set.seed(123)
    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat

    subject_names <- as.character(unique(dat$subjid))
    expect_equal(names(ld$is_mar), subject_names)
    expect_equal(names(ld$is_missing), subject_names)
    expect_equal(ld$ids, subject_names)

    expect_equal(ld$visits,  levels(dat$visit))
    expect_length(ld$strata, length(unique(dat$subjid)))

    expect_equal(
        unlist(ld$is_missing, use.names = FALSE),
        c(
            TRUE, TRUE, TRUE,
            TRUE, FALSE, TRUE,
            TRUE, FALSE, FALSE,
            FALSE, FALSE, FALSE
        )
    )

    expect_equal(
        unlist(ld$is_mar, use.names = FALSE),
        rep(TRUE, dobj$n * dobj$nv)
    )

})



test_that("longData - Sampling", {

    set.seed(145)
    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat

    set.seed(101)
    samps <- replicate(
        n = 1000,
        ld$sample_ids()
    )

    expect_true("1" %in% samps[1, ])
    expect_true("2" %in% samps[1, ])
    expect_true("3" %in% samps[1, ])

    ## Subject "4" is the only subject in their strata so they must be sampled
    expect_true(all(samps[4, ] == "4"))

    ### Looking to see that re-sampling is working i.e. samples contain duplicates
    expect_true(any(apply(samps, 2, function(x) length(unique(x))) %in% c(1, 2)))

    expect_error(
        ld$get_data("-1231"),
        "subjids are not in self"
    )

    x <- ld$get_data(c("1", "1", "3"))

    y <- bind_rows(
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "3")
    )
    expect_equal(
        select(x, -subjid),
        select(y, -subjid) %>% as.data.frame()
    )
    expect_true(all(x$subjid != y$subjid))



    imputes <- imputation_df(
        imputation_single(id = "1", values = c(1, 2, 3)),
        imputation_single(id = "4", values = c()),
        imputation_single(id = "1", values = c(4, 5, 6)),
        imputation_single(id = "2", values = c(7, 8))
    )
    x <- ld$get_data(imputes)
    pt2_val <- dat %>%
        filter(subjid == "2") %>%
        pull(outcome)

    pt2_val[is.na(pt2_val)] <- c(7, 8)

    y <- bind_rows(
        dat %>% filter(subjid == "1") %>% mutate(outcome = c(1, 2, 3)),
        dat %>% filter(subjid == "4"),
        dat %>% filter(subjid == "1") %>% mutate(outcome = c(4, 5, 6)),
        dat %>% filter(subjid == "2") %>% mutate(outcome = pt2_val)
    )

    expect_equal(
        select(x, -subjid),
        select(y, -subjid) %>% as.data.frame()
    )
    expect_true(all(x$subjid != y$subjid))



    x <- ld$get_data(c("1", "1", "1", "2"), na.rm = TRUE)

    pt2_val <- dat %>% filter(subjid == "2") %>% pull(outcome)
    y <- bind_rows(
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "1"),
        dat %>% filter(subjid == "2"),
    ) %>%
        filter(!is.na(outcome))
    expect_equal(
        select(x, -subjid),
        select(y, -subjid) %>% as.data.frame()
    )
    expect_true(all(x$subjid != y$subjid))




    ilist <- imputation_df(
        imputation_single(id = "1", values = c(1, 2)),
        imputation_single(id = "2", values = c(1, 2, 3))
    )

    expect_error(
        ld$get_data(ilist),
        "Number of missing values doesn't equal"
    )

    expect_error(
        ld$get_data(imputation_df(ilist[1])),
        "Number of missing values doesn't equal"
    )

    expect_error(
        ld$get_data(imputation_df(ilist[2])),
        "Number of missing values doesn't equal"
    )
})



test_that("Stratification works as expected", {
    set.seed(102)
    dobj <- get_data(50)
    dat <- dobj$dat
    dat_ice <- dobj$dat_ice
    vars <- dobj$vars

    vars$strata <- "group"

    ld <- longDataConstructor$new(dat, vars)

    real <- dat %>% group_by(group) %>% tally()

    for (i in 1:20) {
        sampled <- ld$get_data(ld$sample_ids()) %>%
            group_by(group) %>%
            tally()
        expect_equal(real, sampled)
    }

    vars$strata <- c("group", "sex")

    ld <- longDataConstructor$new(dat, vars)

    real <- dat %>% group_by(group, sex) %>% tally()

    for (i in 1:20) {
        sampled <- ld$get_data(ld$sample_ids()) %>%
            group_by(group, sex) %>%
            tally()
        expect_equal(real, sampled)
    }
})



test_that("Group is a stratification variable by default", {

    set.seed(5176)
    dobj <- get_data(60)
    dat <- dobj$dat
    dat_ice <- dobj$dat_ice

    vars <- set_vars(
        subjid = "id",
        visit = "visit",
        outcome = "outcome",
        group = "group",
        strategy = "strategy"
    )

    ld <- longDataConstructor$new(dat, vars)
    expect_equal(ld$vars$strata, "group")
    real <- dat %>% group_by(group) %>% tally()
    for (i in 1:20) {
        sampled <- ld$get_data(ld$sample_ids()) %>%
            group_by(group) %>%
            tally()
        expect_equal(real, sampled)
    }



    vars <- set_vars(
        subjid = "id",
        visit = "visit",
        outcome = "outcome",
        group = "sex",
        strategy = "strategy"
    )
    ld <- longDataConstructor$new(dat, vars)
    expect_equal(ld$vars$strata, "sex")
    real <- dat %>% group_by(sex) %>% tally()
    for (i in 1:20) {
        sampled <- ld$get_data(ld$sample_ids()) %>%
            group_by(sex) %>%
            tally()
        expect_equal(real, sampled)
    }
})



test_that("Strategies", {

    set.seed(178)
    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat

    expect_equal(
        unlist(ld$strategies, use.names = FALSE),
        rep("MAR", dobj$n)
    )

    expect_equal(
        unlist(ld$ice_visit_index, use.names = FALSE),
        rep(4, dobj$n)
    )

    dat_ice <- tribble(
        ~visit, ~subjid, ~strategy,
        "Visit 1", "1",  "ABC",
        "Visit 2",  "2",  "MAR",
        "Visit 3",  "3",  "XYZ"
    )

    ld$set_strategies(dat_ice)

    expect_equal(
        unlist(ld$strategies, use.names = FALSE),
        c("ABC", "MAR", "XYZ", "MAR")
    )

    expect_equal(
        unlist(ld$strategy_lock, use.names = FALSE),
        c(FALSE, TRUE, TRUE, FALSE)
    )

    expect_equal(
        unlist(ld$is_mar, use.names = FALSE),
        c(
            FALSE, FALSE, FALSE,
            TRUE, TRUE, TRUE,
            TRUE, TRUE, FALSE,
            TRUE, TRUE, TRUE
        )
    )

    expect_equal(
        unlist(ld$ice_visit_index, use.names = FALSE),
        c(1, 2, 3, 4)
    )

    dat_ice <- tribble(
        ~subjid, ~strategy,
        "1",  "ABC",
        "2",  "MAR",
        "3",  "ABC"
    )
    ld$update_strategies(dat_ice)

    expect_equal(
        unlist(ld$ice_visit_index, use.names = FALSE),
        c(1, 2, 3, 4)
    )

    expect_equal(
        unlist(ld$is_mar, use.names = FALSE),
        c(
            FALSE, FALSE, FALSE,
            TRUE, TRUE, TRUE,
            TRUE, TRUE, FALSE,
            TRUE, TRUE, TRUE
        )
    )

    expect_equal(
        unlist(ld$strategies, use.names = FALSE),
        c("ABC", "MAR", "ABC", "MAR")
    )

    dat_ice <- tribble(
        ~visit, ~subjid, ~strategy,
        "Visit 1", "2",  "ABC",
    )
    expect_error(
        ld$update_strategies(dat_ice),
        "MAR to non-MAR is invalid"
    )

    dat_ice <- tribble(
        ~subjid, ~strategy,
        "3",  "MAR",
    )

    expect_warning(
        ld$update_strategies(dat_ice),
        "from non-MAR to MAR"
    )


    # Ensure that only 1 warning is issued when converting non-MAR to MAR data
    dat_ice <- tribble(
        ~visit, ~subjid, ~strategy,
        "Visit 1", "1",  "ABC",
        "Visit 1",  "2",  "ABC",
        "Visit 3",  "3",  "XYZ"
    )

    ld$set_strategies(dat_ice)

    upd_dat_ice <- tribble(
        ~subjid, ~strategy,
        "2",  "MAR",
        "3",  "MAR",
    )

    recorded_result <- record(ld$update_strategies(upd_dat_ice))
    expect_length(recorded_result$warnings, 1)
    expect_length(recorded_result$errors, 0)
    expect_true(grepl("Updating strategies from non-MAR to MAR", recorded_result$warnings))
})




test_that("strategies part 2", {

    # Here we check to see that using `update_strategies` only updates the strategy and not
    # the visits (or anything else for that matter)

    set.seed(987)
    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat


    dat_ice <- tribble(
        ~visit, ~subjid, ~strategy,
        "Visit 1", "1",  "ABC",
        "Visit 2",  "2",  "MAR",
        "Visit 3",  "3",  "XYZ"
    )

    ld$set_strategies(dat_ice)
    pre_update_ld <- ld_2_list(ld)


    dat_ice <- tribble(
        ~subjid, ~strategy, ~visit,
        "1", "ABC", "Visit 2",
        "2", "MAR", "Visit 7",
        "3", "XYZ", "Visit 1"
    )
    ld$update_strategies(dat_ice)
    expect_equal(ld_2_list(ld), pre_update_ld)




    dat_ice <- tribble(
        ~subjid, ~strategy, ~visit,
        "1", "LKJ", "Visit 2",
        "2", "MAR", "Visit 7",
        "3", "XYZ", "Visit 1"
    )

    ld$update_strategies(dat_ice)

    expect_equal(
        ld$is_mar,
        pre_update_ld$is_mar
    )

    expect_equal(
        ld$ice_visit_index,
        pre_update_ld$ice_visit_index
    )

    expect_equal(
        unlist(ld$strategies, use.names = FALSE),
        c("LKJ", "MAR", "XYZ", "MAR")
    )


    #### Show that not setting an ICE doesn't affect the ice_visit_index
    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat
    ld$set_strategies()

    dat_ice <- tribble(
        ~subjid, ~strategy, ~visit,
        "1", "LKJ", "Visit 2",
        "2", "MAR", "Visit 7",
        "3", "XYZ", "Visit 1"
    )
    ld$update_strategies(dat_ice)

    expect_equal(
        unlist(ld$ice_visit_index, use.names = FALSE),
        c(4, 4, 4, 4)
    )
    expect_equal(
        unlist(ld$strategies, use.names = FALSE),
        c("LKJ", "MAR", "XYZ", "MAR")
    )

})





test_that("sample_ids", {
    set.seed(101)
    x <- sample_ids(c(1, 2, 3))
    set.seed(101)
    y <- sample_ids(c(1, 2, 3))
    set.seed(7)
    z <- sample_ids(c(1, 2, 3))

    expect_equal(x, y)
    expect_true(all(x %in% c(1, 2, 3)))
    expect_true(all(z %in% c(1, 2, 3)))
    expect_length(x, 3)
    expect_length(y, 3)
    expect_length(z, 3)

    set.seed(200)
    samps <- replicate(
        n = 10000,
        sample_ids(c(1, 2, 3))
    )

    ### Looking to see that re-sampling is working i.e. samples contain duplicates
    expect_true(any(apply(samps, 2, function(x) length(unique(x))) %in% c(1, 2)))

    ### Assuming random sampling the mean should converge to ~2
    samps_mean <- apply(samps, 1, mean)
    expect_true(all(samps_mean >= 1.95 & samps_mean <= 2.05))
})


test_that("as_strata", {
    expect_equal(as_strata(c(1, 2, 3), c(1, 2, 3)), c(1, 2, 3))
    expect_equal(as_strata(c(1, 1, 2), c(5, 5, 6)), c(1, 1, 2))
    expect_equal(as_strata(c(1, 1, 1), c("a", "a", "a")), c(1, 1, 1))
    expect_equal(as_strata(c("a", "b", "c"), c("a", "a", "a")), c(1, 2, 3))
    expect_equal(as_strata(c("a", "a", "c"), c("a", "a", "a")), c(1, 1, 2))
})



test_that("idmap", {
    # The idmap option provides a mapping vectoring linking new_ids to old_ids
    set.seed(654)
    dobj <- get_ld()
    ld <- dobj$ld
    dat <- dobj$dat

    x <- ld$get_data(c("1", "1", "3"), idmap = TRUE)
    expect_equal(
        attr(x, "idmap"),
        c("new_pt_1" = "1", "new_pt_2" = "1", "new_pt_3" = "3")
    )

    x <- ld$get_data(c("1", "1", "3"), idmap = TRUE, na.rm = TRUE)
    expect_equal(
        attr(x, "idmap"),
        c("new_pt_1" = "1", "new_pt_2" = "1", "new_pt_3" = "3")
    )

    imps <- imputation_df(list(
        imputation_single(id = "1", values = c(1, 2, 3)),
        imputation_single(id = "3", values = c(4)),
        imputation_single(id = "3", values = 5)
    ))
    x <- ld$get_data(imps, idmap = TRUE)
    expect_equal(
        attr(x, "idmap"),
        c("new_pt_1" = "1", "new_pt_2" = "3", "new_pt_3" = "3")
    )
})



test_that("longdata can handle data that isn't sorted", {

    dat <- tibble(
        visit = factor(c("v1", "v2", "v3", "v3", "v1", "v2"), levels = c("v1", "v2", "v3")),
        id = factor(c("1", "1", "1", "2", "2", "2")),
        group = factor(c("A", "A", "A", "B", "B", "B")),
        outcome = c(1, 2, 3, 4, 5, NA)
    )

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        strategy = "strategy"
    )

    dat_ice <- tibble(
        visit = "v2",
        id = "2",
        strategy = "JR"
    )

    ld <- longDataConstructor$new(
        data = dat,
        vars = vars
    )
    ld$set_strategies(dat_ice)

    expect_equal(ld$values, list("1" = c(1, 2, 3), "2" = c(5, NA, 4)))
    expect_equal(ld$is_missing, list("1" = c(FALSE, FALSE, FALSE), "2" = c(FALSE, TRUE, FALSE)))
    expect_equal(ld$is_mar, list("1" = c(TRUE, TRUE, TRUE), "2" = c(TRUE, FALSE, FALSE)))

    dat2 <- dat %>%
        arrange(id, visit) %>%
        as_dataframe()

    expect_equal(
        dat2,
        ld$get_data()
    )
})




test_that("longdata rejects data that has no useable observations for a visit", {

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        strategy = "strategy"
    )

    dat <- tibble(
        visit = factor(c("v1", "v2", "v3", "v1", "v2", "v3"), levels = c("v1", "v2", "v3")),
        id = factor(c("1", "1", "1", "2", "2", "2")),
        group = factor(c("A", "A", "A", "B", "B", "B")),
        outcome = c(1, 2, NA, 4, 5, NA)
    )

    expect_error(
        longDataConstructor$new(data = dat, vars = vars),
        regexp = "resulted in the `v3` visit"
    )

    dat <- tibble(
        visit = factor(c("v1", "v2", "v3", "v1", "v2", "v3"), levels = c("v1", "v2", "v3")),
        id = factor(c("1", "1", "1", "2", "2", "2")),
        group = factor(c("A", "A", "A", "B", "B", "B")),
        outcome = c(1, 2, 3, 4, 5, NA)
    )

    dat_ice <- tibble(
        visit = "v2",
        id = c("2", "1"),
        strategy = "JR"
    )

    ld <- longDataConstructor$new(data = dat, vars = vars)
    expect_error(
        ld$set_strategies(dat_ice),
        regexp = "has resulted in the `v2`, `v3` visit"
    )

})



test_that(
    "Validate `is_mar` object", {

        index_mar <- as_class(c(TRUE, TRUE, FALSE, FALSE), "is_mar")
        expect_true(validate(index_mar))

        index_mar <- as_class(c(TRUE, TRUE, TRUE, TRUE), "is_mar")
        expect_true(validate(index_mar))

        index_mar <- as_class(c(FALSE, FALSE, FALSE, FALSE), "is_mar")
        expect_true(validate(index_mar))

        index_mar <- as_class(c(TRUE, TRUE, FALSE, TRUE), "is_mar")
        expect_error(validate(index_mar))

        index_mar <- as_class(c(FALSE, FALSE, TRUE, TRUE), "is_mar")
        expect_error(validate(index_mar))

    }
)



test_that("Formula is created properly", {

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "subjid",
        group = "group",
        strata = "strata",
        covariates = c("sex", "age"),
        strategy = "strategy"
    )

    dat <- tibble(
        subjid = factor(rep(c("Tom", "Harry", "Phil", "Ben"), each = 3), levels = c("Tom", "Harry", "Phil", "Ben")),
        age = rep(c(0.04, -0.14, -0.03, -0.33), each = 3),
        group = factor(rep(c("B", "B", "A", "A"), each = 3), levels = c("A", "B")),
        sex = factor(rep(c("F", "M", "M", "F"), each = 3), levels = c("M", "F")),
        strata = rep(c("A", "A", "A", "B"), each = 3),
        visit = factor(rep(c("Visit 1", "Visit 2", "Visit 3"), 4)),
        outcome = c(
            NA, NA, NA,
            NA, 4.14, NA,
            NA, -1.34, 2.41,
            -1.53, 1.03, 2.58
        )
    )
    ld <- longDataConstructor$new(
        data = dat,
        vars = vars
    )
    formula_actual <- outcome ~ 1 + group + visit + sex + age
    expect_true(formula_actual  == ld$formula)


    dat <- tibble(
        subjid = factor(rep(c("Tom", "Harry", "Phil", "Ben"), each = 3), levels = c("Tom", "Harry", "Phil", "Ben")),
        age = rep(c(0.04, -0.14, -0.03, -0.33), each = 3),
        group = factor(rep(c("B", "B", "B", "B"), each = 3), levels = c("B")),
        sex = factor(rep(c("F", "M", "M", "F"), each = 3), levels = c("M", "F")),
        strata = rep(c("A", "A", "A", "B"), each = 3),
        visit = factor(rep(c("Visit 1", "Visit 2", "Visit 3"), 4)),
        outcome = c(
            NA, NA, NA,
            NA, 4.14, NA,
            NA, -1.34, 2.41,
            -1.53, 1.03, 2.58
        )
    )
    ld <- longDataConstructor$new(
        data = dat,
        vars = vars
    )
    formula_actual <- outcome ~ 1 + visit + sex + age
    expect_true(formula_actual  == ld$formula)


    dat <- tibble(
        subjid = factor(rep(c("Tom", "Harry", "Phil", "Ben"), each = 3), levels = c("Tom", "Harry", "Phil", "Ben")),
        age = rep(c(0.04, -0.14, -0.03, -0.33), each = 3),
        group = factor(rep(c("A", "B", "C", "D"), each = 3), levels = c("A", "B", "C", "D")),
        sex = factor(rep(c("F", "M", "M", "F"), each = 3), levels = c("M", "F")),
        strata = rep(c("A", "A", "A", "B"), each = 3),
        visit = factor(rep(c("Visit 1", "Visit 2", "Visit 3"), 4)),
        outcome = c(
            NA, NA, NA,
            NA, 4.14, NA,
            NA, -1.34, 2.41,
            -1.53, 1.03, 2.58
        )
    )
    ld <- longDataConstructor$new(
        data = dat,
        vars = vars
    )
    formula_actual <- outcome ~ 1 + group + visit + sex + age
    expect_true(formula_actual  == ld$formula)
})




test_that("check_has_data_at_each_visit() catches the correct visit that has no data", {

    visits <- c("V", "I", "S", "T")

    dat <- tibble(
        pt = factor(c("A", "A", "A", "A", "B", "B", "B", "B"), levels = c("A", "B")),
        vis = factor(rep(visits, 2), levels = visits),
        out = c(NA, 4, 5, 3, 6, NA, 1, NA),
        group = factor(c("G", "G", "G", "G", "F", "F", "F", "F"), levels = c("G", "F")),
        age = rnorm(8)
    )

    vars <- set_vars(
        outcome = "out",
        visit = "vis",
        subjid = "pt",
        group = "group",
        covariates = c("age"),
        strategy = "strategy"
    )

    ld <- longDataConstructor$new(dat, vars)

    dat_ice <- tibble(
        vis = factor(c("S", "T"), levels = visits),
        pt = factor(c("A", "B"), levels = c("A", "B")),
        strategy = c("JR", "JR")
    )

    expect_error(
        ld$set_strategies(dat_ice),
        regexp = "`T` visit"
    )



    visits <- c(5, 6, 8, 1)

    dat <- tibble(
        pt = factor(c("A", "A", "A", "A", "B", "B", "B", "B"), levels = c("A", "B")),
        vis = factor(rep(visits, 2), levels = visits),
        out = c(NA, 4, 5, 3, 6, NA, 1, NA),
        group = factor(c("G", "G", "G", "G", "F", "F", "F", "F"), levels = c("G", "F")),
        age = rnorm(8)
    )

    vars <- set_vars(
        outcome = "out",
        visit = "vis",
        subjid = "pt",
        group = "group",
        covariates = c("age"),
        strategy = "strategy"
    )

    ld <- longDataConstructor$new(dat, vars)

    dat_ice <- tibble(
        vis = factor(c(8, 1), levels = visits),
        pt = factor(c("A", "B"), levels = c("A", "B")),
        strategy = c("JR", "JR")
    )

    expect_error(
        ld$set_strategies(dat_ice),
        regexp = "`1` visit"
    )



    ld <- longDataConstructor$new(dat, vars)

    dat_ice <- tibble(
        vis = factor(c(8, 1), levels = visits),
        pt = factor(c("B", "A"), levels = c("A", "B")),
        strategy = c("MAR", "MAR")
    )

    expect_true(ld$set_strategies(dat_ice))

})




test_that("get_data() uses na.rm and nmar.rm correctly", {

    #
    # This test proves that the bug identified in
    # https://github.com/insightsengineering/rbmi/issues/347
    # has been resolved.
    # This was where `na.rm` and `nmar.rm` in `longdata$get_data()` only worked if IDs
    # were passed to the function
    #

    visits <- c("V", "I", "S", "T")

    dat <- tibble(
        pt = factor(c("B", "B", "A", "A", "B", "B", "A", "A"), levels = c("A", "B")),
        vis = factor(c("V", "T", "T", "S", "I", "S", "I", "V"), levels = visits),
        out = c(NA, 4,    5, NA,     6, 5,     5, 5),
        group = factor(c("G", "G", "F", "F", "G", "G", "F", "F"), levels = c("G", "F")),
        age = rnorm(8)
    )

    IDS <- c("A", "B", "A")
    # Dataset to represent what the data should look like if the above IDs are specified
    dat2 <- bind_rows(
        dat %>% arrange(pt, vis),
        dat %>% arrange(pt, vis) %>% filter(pt == "A")
    ) %>%
        mutate(pt = rep(paste0("new_pt_", 1:3), each = 4))


    vars <- set_vars(
        outcome = "out",
        visit = "vis",
        subjid = "pt",
        group = "group",
        covariates = c("age"),
        strategy = "strategy"
    )

    ld <- longDataConstructor$new(dat, vars)

    ### Pre-strategies (no ids) (everything is MAR atm)

    expect_equal(
        ld$get_data(),
        dat %>% arrange(pt, vis) %>% as.data.frame()
    )

    expect_equal(
        ld$get_data(na.rm = TRUE),
        dat %>% filter(!is.na(out)) %>% arrange(pt, vis) %>% as.data.frame()
    )

    expect_equal(
        ld$get_data(na.rm = TRUE, nmar.rm = TRUE),
        dat %>% filter(!is.na(out)) %>% arrange(pt, vis) %>% as.data.frame()
    )

    expect_equal(
        ld$get_data(nmar.rm = TRUE),
        dat %>% arrange(pt, vis) %>% as.data.frame()
    )

    ### Pre-strategies (with ids) (everything is MAR atm)


    expect_equal(
        ld$get_data(IDS),
        dat2 %>% as.data.frame()
    )

    expect_equal(
        ld$get_data(IDS, na.rm = TRUE),
        dat2 %>% filter(!is.na(out)) %>% as.data.frame()
    )

    expect_equal(
        ld$get_data(IDS, nmar.rm = TRUE),
        dat2 %>% as.data.frame()
    )

    expect_equal(
        ld$get_data(IDS, na.rm = TRUE, nmar.rm = TRUE),
        dat2 %>% filter(!is.na(out)) %>% as.data.frame()
    )

    ############
    #
    #  Set strategies and test again....
    #

    dat_ice <- tibble(
        vis = factor(c("I", "S"), levels = visits),
        pt = factor(c("A", "B"), levels = c("A", "B")),
        strategy = c("JR", "MAR")
    )
    ld$set_strategies(dat_ice)


    ### Post-strategies (without ids)

    expect_equal(
        ld$get_data(),
        dat %>%
            arrange(pt, vis) %>%
            as.data.frame()
    )

    expect_equal(
        ld$get_data(na.rm = TRUE),
        dat %>%
            filter(!is.na(out)) %>%
            arrange(pt, vis) %>%
            as.data.frame()
    )

    expect_equal(
        ld$get_data(nmar.rm = TRUE),
        dat %>%
            filter(!(as.numeric(vis) >= 2 & pt == "A")) %>%
            arrange(pt, vis) %>%
            as.data.frame()
    )

    expect_equal(
        ld$get_data(na.rm = TRUE, nmar.rm = TRUE),
        dat %>%
            filter(!is.na(out)) %>%
            filter(!(as.numeric(vis) >= 2 & pt == "A")) %>%
            arrange(pt, vis) %>%
            as.data.frame()
    )

    ### Post-strategies (with ids)

    expect_equal(
        ld$get_data(IDS),
        dat2 %>%
            as.data.frame()
    )

    expect_equal(
        ld$get_data(IDS, na.rm = TRUE),
        dat2 %>%
            filter(!is.na(out)) %>%
            as.data.frame()
    )

    expect_equal(
        ld$get_data(IDS, nmar.rm = TRUE),
        dat2 %>%
            filter(!(as.numeric(vis) >= 2 & pt %in% c("new_pt_1", "new_pt_3"))) %>%
            as.data.frame()
    )

    expect_equal(
        ld$get_data(IDS, na.rm = TRUE, nmar.rm = TRUE),
        dat2 %>%
            filter(!is.na(out)) %>%
            filter(!(as.numeric(vis) >= 2 & pt %in% c("new_pt_1", "new_pt_3"))) %>%
            as.data.frame()
    )

})


test_that("Warnings/errors are thrown when strategies are incorrectly updated", {
    vars <- set_vars(
        outcome = "out",
        group = "group",
        strategy = "strat",
        subjid = "pt",
        visit = "vis",
        covariates = c("age")
    )

    dat <- tibble(
        pt = factor(c("A", "A", "A", "B", "B", "B", "C", "C", "C"), levels = c("A", "B", "C")),
        vis = factor(c("V1", "V2", "V3", "V1", "V2", "V3", "V1", "V2", "V3"), levels = c("V1", "V2", "V3")),
        out = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
        group = factor(c("T", "T", "T", "C", "C", "C", "T", "T", "T"), levels = c("C", "T")),
        age = rnorm(9)
    )
    dat_ice <- tibble(
        pt = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
        vis = factor(c("V2", "V2", "V2"), levels = c("V1", "V2", "V3")),
        strat = c("JR", "MAR", "JR")
    )
    longdata <- longDataConstructor$new(dat, vars)
    longdata$set_strategies(dat_ice)

    # Error if updating MAR -> Non-Mar
    ld2 <- longdata$clone()
    dat_ice_upd <- tibble(
        pt = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
        strat = c("JR", "JR", "JR")
    )
    expect_error(
        ld2$update_strategies(dat_ice_upd),
        regexp = "Updating strategies from MAR to non-MAR is invalid"
    )

    # Warning if updating Non-MAR -> MAR
    ld2 <- longdata$clone()
    dat_ice_upd <- tibble(
        pt = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
        strat = c("JR", "MAR", "MAR")
    )
    expect_warning(
        ld2$update_strategies(dat_ice_upd),
        regexp = "Updating strategies from non-MAR to MAR.*You are advised to re-run `draws\\(\\)`"
    )

    # Same as above but catches niche bug where the warning would be supressed
    # if a correct imputation came after an incorrect
    ld2 <- longdata$clone()
    dat_ice_upd <- tibble(
        pt = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
        strat = c("MAR", "MAR", "JR")
    )
    expect_warning(
        ld2$update_strategies(dat_ice_upd),
        regexp = "Updating strategies from non-MAR to MAR.*You are advised to re-run `draws\\(\\)`"
    )
})


test_that("Missing data_ices are handled correctly", {
    vars <- set_vars(
        outcome = "out",
        group = "group",
        strategy = "strat",
        subjid = "pt",
        visit = "vis",
        covariates = c("age")
    )
    dat <- tibble(
        pt = factor(c("A", "A", "A", "B", "B", "B", "C", "C", "C"), levels = c("A", "B", "C")),
        vis = factor(c("V1", "V2", "V3", "V1", "V2", "V3", "V1", "V2", "V3"), levels = c("V1", "V2", "V3")),
        out = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
        group = factor(c("T", "T", "T", "C", "C", "C", "T", "T", "T"), levels = c("C", "T")),
        age = rnorm(9)
    )
    dat_ice <- tibble(
        pt = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
        vis = factor(c("V2", "V2", "V2"), levels = c("V1", "V2", "V3")),
        strat = c("JR", "MAR", "JR")
    )
    dat_ice <- dplyr::filter(dat_ice, pt == "NOT A PT")

    longdata <- longDataConstructor$new(dat, vars)

    original <- longdata$clone(deep = TRUE)
    longdata$set_strategies(dat_ice)

    # When using a blank data_ice, nothing in longdata should be updated
    expect_equal(longdata, original)
})
