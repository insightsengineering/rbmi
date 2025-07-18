suppressPackageStartupMessages({
    library(dplyr)
})


test_that("Parallisation works as expected", {
    skip_if_not(is_full_test())

    bign <- 150
    sigma <- as_vcov(
        c(2, 1, 0.7, 3, 4),
        c(
            0.3,
            0.4,
            0.2,
            0.5,
            0.3,
            0.2,
            0.1,
            0.2,
            0.3,
            0.5
        )
    )

    dat <- get_sim_data(bign, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(
            outcome = if_else(
                is_miss == 1 & visit == "visit_3",
                NA_real_,
                outcome
            )
        ) %>%
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

    test_parallel <- function(method, ncores = 2) {
        set.seed(101)
        time_1_core <- time_it({
            results_1 <- draws(
                data = dat,
                data_ice = dat_ice,
                vars = vars,
                method = method,
                ncores = 1,
                quiet = TRUE
            )
        })

        set.seed(101)
        time_2_core <- time_it({
            results_2 <- draws(
                data = dat,
                data_ice = dat_ice,
                vars = vars,
                method = method,
                ncores = ncores,
                quiet = TRUE
            )
        })

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


test_that("Basic parallisation works as expected", {
    set.seed(3812)
    bign <- 100
    sigma <- as_vcov(
        c(2, 1, 0.7),
        c(
            0.3,
            0.4,
            0.2
        )
    )
    dat <- get_sim_data(bign, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(
            outcome = if_else(
                is_miss == 1 & visit == "visit_3",
                NA_real_,
                outcome
            )
        ) %>%
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
    x1 <- draws(
        quiet = TRUE,
        dat,
        dat_ice,
        vars,
        method = method_approxbayes(n_samples = 10)
    )

    set.seed(3013)
    x2 <- draws(
        quiet = TRUE,
        dat,
        dat_ice,
        vars,
        ncores = 2,
        method = method_approxbayes(n_samples = 10)
    )

    # Tolerance is set here to address mmrm issue where the first optimiser fails when run
    # in parallel and moves onto the second optimiser, where as in sequence the first
    # optimiser works fine. i.e. results are slightly different due to different optimisers
    # being used
    # https://github.com/openpharma/mmrm/issues/151
    expect_equal(x1, x2, tolerance = 0.0001)
})


###########################
#
#  Manual time testing
#
# method <- method_approxbayes(n_samples = 20)
# method <- method_condmean(n_samples = 80)
# method <- method_condmean(type = "jackknife")
# time_it({
#     results_2 <- draws(
#         data = dat,
#         data_ice = dat_ice,
#         vars = vars,
#         method = method,
#         ncores = 1
#     )
# })
# time_it({
#     results_2 <- draws(
#         data = dat,
#         data_ice = dat_ice,
#         vars = vars,
#         method = method,
#         ncores = 2
#     )
# })

test_that("Creation and management of user defined clusters works as expected", {
    # Setup a function to be run on the parallel cluster that requires
    # global objects (namely the `inner_fun` and environment `e`) as well
    # as a handful of packages to be loaded
    e <- new.env()
    e$x <- 20
    inner_fun <- function(x) {
        local_env <- e
        temp1 <- e$x + 0
        temp2 <- rnorm(2)
        temp3 <- dplyr::as_tibble(dplyr::starwars) # Explicit namespace
        temp4 <- day(x) # lubridate::day()
        e$x
    }
    outer_fun <- function(x) {
        temp1 <- inner_fun(2)
        temp2 <- anova.lme %>% invisible() # nlme::anova.lme()
        temp3 <- iris
        temp1 + x + 10
    }

    # Check that function can be run (e.g. all elements are correctly exported)
    set.seed(1223)
    cl1 <- make_rbmi_cluster(
        2,
        list(inner_fun = inner_fun, e = e),
        c("lubridate", "nlme", "dplyr")
    )
    res_1_a <- parallel::clusterCall(cl1, rnorm, 200)
    res_1_b <- parallel::clusterApplyLB(cl1, c(4, 5), outer_fun)
    expect_equal(res_1_b, list(34, 35))

    # Test that re-using an existing cluster is quick to load
    time <- time_it({
        set.seed(1223)
        cl2 <- make_rbmi_cluster(
            cl1,
            list(inner_fun = inner_fun, e = e),
            c("lubridate", "nlme")
        )
    })
    expect_true(as.numeric(time) <= 2)

    # Should produce identical results as before
    res_2_a <- parallel::clusterCall(cl2, rnorm, 200)
    res_2_b <- parallel::clusterApplyLB(cl2, c(4, 5), outer_fun)
    expect_equal(res_2_a, res_1_a)
    expect_equal(res_2_b, res_1_b)

    # Both clusters should be closed as they are points to the same thing
    parallel::stopCluster(cl2)
    expect_true(is_cluster_closed(cl1))
    expect_true(is_cluster_closed(cl2))

    # Check that seed ensures reproducibility
    set.seed(1223)
    cl <- make_rbmi_cluster(
        2,
        list(inner_fun = inner_fun, e = e),
        c("lubridate", "nlme")
    )
    res_3_a <- parallel::clusterCall(cl1, rnorm, 200)
    expect_equal(res_3_a, res_1_a)
    parallel::stopCluster(cl)
})
