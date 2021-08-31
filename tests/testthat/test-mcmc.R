suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
    library(glmmTMB)
})


test_that("split_dim creates a list from an array as expected", {
    mat <- rbind(c(1, 0.2), c(0.2, 1))
    a <- array(data = NA, dim = c(3, 2, 2))
    for(i in 1:dim(a)[1]) {
        a[i, , ] <- mat + i - 1
    }

    actual_res <- split_dim(a, 1)
    expected_res <- list(mat, mat + 1, mat + 2)
    expect_equal(actual_res, expected_res)
})


# Warnings management
# Posterior == MMRM



test_that("Verbose supression works", {

    set.seed(301)
    sigma <- as_covmat(c(6, 4, 4), c(0.5, 0.2, 0.3))
    dat <- get_sim_data(50, sigma)

    dat_ice <- dat %>%
        group_by(id) %>%
        arrange(desc(visit)) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(strategy = "MAR")

    vars <- ivars(
        visit = "visit",
        subjid = "id",
        group = "group",
        covariates = "sex",
        strategy = "strategy",
        outcome = "outcome"
    )

    suppressWarnings({
        msg <- capture.output({
            x <- draws(dat, dat_ice, vars, method_bayes(n_samples = 2, verbose = TRUE))
        })
    })
    expect_true(length(msg) > 0)


    suppressWarnings({
        msg <- capture.output({
            x <- draws(dat, dat_ice, vars, method_bayes(n_samples = 2, verbose = FALSE))
        })
    })
    expect_true(length(msg) == 0)
})




test_that("as_indicies", {

    result_actual <- as_indices(c("1100"))
    result_expected <- list(c(1, 2, 0, 0))
    expect_equal(result_actual, result_expected)


    result_actual <- as_indices(c(
        "10101",
        "11010",
        "00000",
        "11111",
        "01111",
        "11110",
        "10111",
        "10000",
        "01000",
        "00001"
    ))

    result_expected <- list(
        c(1, 3, 5, 0, 0),
        c(1, 2, 4, 0, 0),

        c(0, 0, 0, 0, 0),
        c(1, 2, 3, 4, 5),

        c(2, 3, 4, 5, 0),
        c(1, 2, 3, 4, 0),
        c(1, 3, 4, 5, 0),

        c(1, 0, 0, 0, 0),
        c(2, 0, 0, 0, 0),
        c(5, 0, 0, 0, 0)
    )
    expect_equal(result_actual, result_expected)

    expect_error(as_indices(c("12")), "must be 0 or 1")
    expect_error(as_indices(c("11", "1")), "same length")
    expect_error(as_indices(c("11", "111")), "same length")

})





test_that("get_pattern_groups", {
    dat <- tibble(
        V1 = c(1, 2, 3, 4, 5, 6),
        V2 = c(9, 8, 7, 6, 5, 6),
        subjid = c("1", "1", "1", "2", "2", "2"),
        visit = c(1, 2, 3, 1, 2, 3),
        group = c(1, 1, 1, 1, 1, 1),
        is_avail = c(1, 1, 0, 1, 0, 1)
    )
    results_actual <- get_pattern_groups(dat) %>% as_tibble()
    results_expected <- tibble(
        subjid = c("1", "2"),
        group = c(1, 1),
        pattern = c("110", "101"),
        pgroup = c(1, 2)
    )
    expect_equal(results_actual, results_expected)



    dat <- tibble(
        subjid = c("1", "1", "2", "2", "3", "3"),
        visit = c(1, 2, 1, 2, 1, 2),
        group = c(1, 1, 2, 2, 3, 3),
        is_avail = c(1, 1, 1, 1, 1, 1)
    )
    results_actual <- get_pattern_groups(dat) %>% as_tibble()
    results_expected <- tibble(
        subjid = c("1", "2", "3"),
        group = c(1, 2, 3),
        pattern = c("11", "11", "11"),
        pgroup = c(1, 2, 3)
    )
    expect_equal(results_actual, results_expected)



    dat <- tibble(
        subjid = c("1", "1", "2", "2", "3", "3"),
        visit = c(1, 2, 1, 2, 1, 2),
        group = c(1, 1, 2, 2, 2, 2),
        is_avail = c(1, 1, 1, 1, 1, 1)
    )
    results_actual <- get_pattern_groups(dat) %>% as_tibble()
    results_expected <- tibble(
        subjid = c("1", "2", "3"),
        group = c(1, 2, 2),
        pattern = c("11", "11", "11"),
        pgroup = c(1, 2, 2)
    )
    expect_equal(results_actual, results_expected)



    dat <- tibble(
        subjid = c("1", "2", "3", "1", "2", "3"),
        visit = c(1, 1, 1, 2, 2, 2),
        group = c(1, 2, 2, 1, 2, 2),
        is_avail = c(1, 1, 1, 1, 0, 1)
    )
    results_actual <- get_pattern_groups(dat) %>% as_tibble()
    results_expected <- tibble(
        subjid = c("1", "2", "3"),
        group = c(1, 2, 2),
        pattern = c("11", "10", "11"),
        pgroup = c(1, 2, 3)
    )
    expect_equal(results_actual, results_expected)
})






test_that("get_pattern_groups_unique", {
    dat <- tibble(
        subjid = c("1", "2", "4", "5"),
        group = factor(c("a", "a", "a", "a")),
        pattern = c("11", "11", "11", "11"),
        pgroup = c(1, 1, 1, 1)
    )
    results_actual <- get_pattern_groups_unique(dat) %>% as_tibble()
    results_expected <- tibble(
        pgroup = c(1),
        pattern = c("11"),
        group_n = c(1),
        n = c(4),
        n_avail = c(2)
    )
    expect_equal(results_actual, results_expected)



    dat <- tibble(
        subjid = c("1", "2", "4", "5"),
        group = factor(c("a", "a", "a", "b")),
        pattern = c("11", "11", "11", "10"),
        pgroup = c(1, 1, 1, 2)
    )
    results_actual <- get_pattern_groups_unique(dat) %>% as_tibble()
    results_expected <- tibble(
        pgroup = c(1, 2),
        pattern = c("11", "10"),
        group_n = c(1, 2),
        n = c(3,1),
        n_avail = c(2, 1)
    )
    expect_equal(results_actual, results_expected)



    dat <- tibble(
        subjid = c("1", "2", "4", "5"),
        group = factor(c("a", "a", "b", "b")),
        pattern = c("11", "11", "11", "10"),
        pgroup = c(1, 1, 2, 3)
    )
    results_actual <- get_pattern_groups_unique(dat) %>% as_tibble()
    results_expected <- tibble(
        pgroup = c(1, 2, 3),
        pattern = c("11", "11", "10"),
        group_n = c(1, 2, 2),
        n = c(2, 1, 1),
        n_avail = c(2, 2, 1)
    )
    expect_equal(results_actual, results_expected)



    dat <- tibble(
        subjid = c("1", "2", "4", "5"),
        group = factor(c("b", "a", "a", "b")),
        pattern = c("00", "11", "11", "10"),
        pgroup = c(3, 1, 1, 2)
    )
    results_actual <- get_pattern_groups_unique(dat) %>% as_tibble()
    results_expected <- tibble(
        pgroup = c(1, 2, 3),
        pattern = c("11", "10", "00"),
        group_n = c(1, 2, 2),
        n = c(2, 1, 1),
        n_avail = c(2, 1, 0)
    )
    expect_equal(results_actual, results_expected)
})








prepare_stan_data <- function(ddat, subjid, visit, outcome, group)
