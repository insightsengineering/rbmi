suppressPackageStartupMessages({
    library(dplyr)
    library(future)
})


test_that("Parallisation works as expected", {

    skip_if_not(is_full_test())
    plan(sequential)
    bign <- 150
    sigma <- as_vcov(
        c(2, 1, 0.7, 3, 4),
        c(
            0.3,
            0.4, 0.2,
            0.5, 0.3, 0.2,
            0.1, 0.2, 0.3, 0.5
        )
    )

    dat <- get_sim_data(bign, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss)

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

    ###########################
    #
    #  Potential Unit tests
    #

    test_parallel <- function(method, n_node = 2) {
        set.seed(101)
        plan(sequential)
        time_1_core <- time_it({
            results_1 <- draws(
                data = dat,
                data_ice = dat_ice,
                vars = vars,
                method = method,
                quiet = TRUE
            )
        })

        plan(multisession, workers = n_node)
        set.seed(101)
        time_2_core <- time_it({
            results_2 <- draws(
                data = dat,
                data_ice = dat_ice,
                vars = vars,
                method = method,
                quiet = TRUE
            )
        })
        plan(sequential)

        results_1$formula <- x ~ a + b + c + d
        results_2$formula <- x ~ a + b + c + d

        # Test is currently disabled as for some reason there is no performance gains
        # when run on github actions, this test appears to run fine everywhere
        # else though...
        # expect_true(time_1_core > (time_2_core * 1.3))
        expect_equal(results_1, results_2)

        res <- list(
            results_1 = results_1,
            results_2 = results_2,
            time_1 = time_1_core,
            time_2 = time_2_core
        )
        return(res)
    }

    x1 <- test_parallel(method_approxbayes(n_samples = 120))
    x2 <- test_parallel(method_condmean(n_samples = 120))
    x3 <- test_parallel(method_condmean(type = "jackknife"))
    x4 <- test_parallel(method_bmlmi(B = 70, D = 2))

})


test_that("Basic parallisation is reproducible", {
    set.seed(3812)
    plan(sequential)
    bign <- 100
    sigma <- as_vcov(
        c(2, 1, 0.7),
        c(
            0.3,
            0.4, 0.2
        )
    )
    dat <- get_sim_data(bign, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss)

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

    set.seed(3013)
    plan(sequential)
    x1 <- draws(
        quiet = TRUE,
        dat,
        dat_ice,
        vars,
        method = method_approxbayes(n_samples = 15)
    )

    set.seed(3013)
    plan(multisession, workers = 2)
    x2 <- draws(
        quiet = TRUE,
        dat,
        dat_ice,
        vars,
        method = method_approxbayes(n_samples = 15)
    )

    set.seed(11122)
    plan(multisession, workers = 2)
    x3 <- draws(
        quiet = TRUE,
        dat,
        dat_ice,
        vars,
        method = method_approxbayes(n_samples = 15)
    )
    plan(sequential)

    expect_equal(x1, x2, tolerance = 0.0001)
    expect_true(identical(x1$samples, x2$samples))
    expect_false(identical(x1$samples, x3$samples))

})


###########################
#
#  Manual time testing
#
# method <- method_approxbayes(n_samples = 20)
# method <- method_condmean(n_samples = 80)
# method <- method_condmean(type = "jackknife")
# plan(sequential)
# time_it({
#     results_2 <- draws(
#         data = dat,
#         data_ice = dat_ice,
#         vars = vars,
#         method = method
#     )
# })
# plan(multisession, workers = 2)
# time_it({
#     results_2 <- draws(
#         data = dat,
#         data_ice = dat_ice,
#         vars = vars,
#         method = method
#     )
# })



test_that("parallelisation of analyse() works as expected", {
    set.seed(3812)
    plan(sequential)
    bign <- 100
    sigma <- as_vcov(
        c(2, 1, 0.7),
        c(
            0.3,
            0.4, 0.2
        )
    )
    dat <- get_sim_data(bign, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss)

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

    set.seed(3013)
    plan(sequential)
    draw_obj <- draws(
        quiet = TRUE,
        dat,
        dat_ice,
        vars,
        method = method_approxbayes(n_samples = 15)
    )

    impute_obj <- impute(
        draw_obj,
        references = c("A" = "B", "B" = "B")
    )

    new_vars <- set_vars(
        subjid = "id",
        outcome = "outcome",
        visit = "visit",
        group = "group",
        covariates = c("sex", "age")
    )


    # Show that running in parallel vs sequential produce same results
    set.seed(1021)
    system.time({
        plan(sequential)
        ana_obj_1 <- analyse(
            impute_obj,
            vars = new_vars
        )
    })

    set.seed(1021)
    system.time({
        plan(multisession, workers = 2)
        ana_obj_2 <- analyse(
            impute_obj,
            vars = new_vars
        )
    })

    expect_equal(ana_obj_1, ana_obj_2)


    # Check that user defined functions work as expected
    library(lubridate)
    global_val <- 20
    my_a_fun <- function(...) {
        zz <- days(3)
        list(
            "trt_visit_1" = list(
                "est" = global_val,
                "df" = 1,
                "se" = 2
            )
        )
    }
    # Check that automatic library detection works as expected
    system.time({
        plan(multisession, workers = 2)
        ana_obj_2 <- analyse(
            impute_obj,
            fun = my_a_fun
        )
    })

    # Check that manual library / global specification works as expected
    system.time({
        plan(multisession, workers = 2)
        ana_obj_2 <- analyse(
            impute_obj,
            fun = my_a_fun,
            .globals = c("global_val"),
            .packages = "lubridate"
        )
    })

})
