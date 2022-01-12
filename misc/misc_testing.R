


###########################
#
#  Setup - Part 1 - parallel testing
#

suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})

pkgload::load_all()

bign <- 100
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
            ncores = 1
        )
    })

    set.seed(101)
    time_2_core <- time_it({
        results_2 <- draws(
            data = dat,
            data_ice = dat_ice,
            vars = vars,
            method = method,
            ncores = ncores
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

x <- test_parallel(method_approxbayes(n_samples = 80))
x <- test_parallel(method_condmean(n_samples = 80))
x <- test_parallel(method_condmean(type = "jackknife"))


###########################
#
#  Manual time testing
#


method <- method_approxbayes(n_samples = 20)
method <- method_condmean(n_samples = 80)
method <- method_condmean(type = "jackknife")

time_it({
      results_2 <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method,
        ncores = 1
    )
})


time_it({
      results_2 <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method,
        ncores = 2
    )
})







###########################
#
#  Setup - Part 2 - Failure testing
#


bign <- 75
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


MockLongData <- R6Class("MockLongData",
    inherit = longDataConstructor,
    public = list(
        failure_index = 0,
        tracker = 0,
        sample_ids = function() {
            self$tracker = self$tracker + 1
            if (self$tracker %in% self$failure_index) {
                return(c("1", "2"))
            }
            super$sample_ids()
        },
        set_failed_sample_index = function(x) {
            self$tracker = 0
            self$failure_index = x
        }
    )
)


ld <- MockLongData$new(dat, vars)
ld$set_strategies(dat_ice)


##################
#
# Bootstrap 1 core
#
#


method <- method_approxbayes(n_samples = 10, threshold = 0.5)
ld$set_failed_sample_index(seq_len(4))
stack <- get_sample_stack("bootstrap", ld, method)
x <- get_draws_mle(
    longdata = ld,
    method = method,
    n_target_samples = method$n_samples,
    failure_limit = (method$threshold * method$n_samples),
    use_samp_ids = FALSE,
    ncores = 1,
    first_sample_orig = FALSE,
    sample_stack = stack
)
expect_equal(x$n_failures, 4)
expect_equal(ld$tracker, 15)
expect_length(stack$stack, 1)



method <- method_approxbayes(n_samples = 10, threshold = 0.3)
ld$set_failed_sample_index(2:5)
stack <- get_sample_stack("bootstrap", ld, method)
expect_error(
    get_draws_mle(
        longdata = ld,
        method = method,
        n_target_samples = method$n_samples,
        failure_limit = (method$threshold * method$n_samples),
        use_samp_ids = FALSE,
        ncores = 1,
        first_sample_orig = FALSE,
        sample_stack = stack
    )
)
expect_equal(ld$tracker, 13)
expect_length(stack$stack, 8)



##################
#
# Bootstrap 2 core
#
#

method <- method_approxbayes(n_samples = 10, threshold = 0.5)
ld$set_failed_sample_index(seq_len(4))
stack <- get_sample_stack("bootstrap", ld, method)
x <- get_draws_mle(
    longdata = ld,
    method = method,
    n_target_samples = method$n_samples,
    failure_limit = (method$threshold * method$n_samples),
    use_samp_ids = FALSE,
    ncores = 2,
    first_sample_orig = FALSE,
    sample_stack = stack
)
expect_equal(x$n_failures, 4)
expect_equal(ld$tracker, 15)
expect_length(stack$stack, 1)




method <- method_approxbayes(n_samples = 10, threshold = 0.3)
ld$set_failed_sample_index(2:5)
stack <- get_sample_stack("bootstrap", ld, method)
expect_error(
    get_draws_mle(
        longdata = ld,
        method = method,
        n_target_samples = method$n_samples,
        failure_limit = (method$threshold * method$n_samples),
        use_samp_ids = FALSE,
        ncores = 2,
        first_sample_orig = FALSE,
        sample_stack = stack
    )
)
expect_equal(ld$tracker, 13)
expect_length(stack$stack, 13 - 6)



##################
#
# Jackknife - failures only
#
#

method <- method_condmean(type = "jackknife")
stack <- get_sample_stack("jackknife", ld, method)
for (i in 5:10) {
    stack$stack[[i]] <- c("1", "2")
}
expect_error(
    x <- get_draws_mle(
        longdata = ld,
        method = method,
        n_target_samples = length(ld$ids),
        failure_limit = 0,
        use_samp_ids = FALSE,
        ncores = 1,
        first_sample_orig = FALSE,
        sample_stack = stack
    )
)
expect_length(stack$stack, length(ld$ids) - 5)


method <- method_condmean(type = "jackknife")
stack <- get_sample_stack("jackknife", ld, method)
for (i in 5:10) {
    stack$stack[[i]] <- c("1", "2")
}
expect_error(
    x <- get_draws_mle(
        longdata = ld,
        method = method,
        n_target_samples = length(ld$ids),
        failure_limit = 0,
        use_samp_ids = FALSE,
        ncores = 2,
        first_sample_orig = FALSE,
        sample_stack = stack
    )
)
expect_length(stack$stack, length(ld$ids) - 6)