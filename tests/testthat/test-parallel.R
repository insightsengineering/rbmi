suppressPackageStartupMessages({
    library(dplyr)
})


test_that("Parallisation works as expected", {

    skip_if_not(is_nightly())

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
        expect_true(time_1_core > (time_2_core * 1.3))
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