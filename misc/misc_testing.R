

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


test_parallel <- function(method) {
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
            ncores = 2
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



profvis::profvis({
    results_2 <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_approxbayes(n_samples = 80),
        ncores = 1
    )
})


method <- method_approxbayes(n_samples = 80)
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

MockStack <- R6Class("MockStack",
    inherit = Stack,
    public = list(
        tracker = 0,
        pop = function(...) {
            x = super$pop(...)
            self$tracker = self$tracker + length(x)
            return(x)
        },
        reset = function(){
            self$tracker = 0
            stack = list()
        }
    )
)

stack <- MockStack$new()

ld <- MockLongData$new(dat, vars)
ld$set_strategies(dat_ice)

ld$set_failed_sample_index(seq_len(4))
x <- get_bootstrap_draws(
    longdata = ld,
    method = method_approxbayes(n_samples = 10, threshold = 0.5),
    use_samp_ids = FALSE,
    ncores = 1,
    stack = stack
)
expect_equal(x$n_failures, 4)
expect_equal(ld$tracker, 14 + 1)  # +1 for original sample
expect_equal(stack$tracker, 14)



ld$set_failed_sample_index(seq_len(3))
stack$reset()
expect_error(
    get_bootstrap_draws(
        longdata = ld,
        method = method_approxbayes(n_samples = 20, threshold = 0.1),
        use_samp_ids = FALSE,
        ncores = 1,
        stack = stack
    ),
    regexp = "Try using a simpler covariance"
)
expect_equal(ld$tracker, 23)  # +1 for original sample
expect_equal(stack$tracker, 4)




ld$set_failed_sample_index(c(1:4))
x <- get_bootstrap_draws(
    longdata = ld,
    method = method_approxbayes(n_samples = 10, threshold = 0.5),
    use_samp_ids = FALSE,
    ncores = 2
)
expect_equal(x$n_failures, 4)
expect_equal(ld$tracker, 15)


ld$set_failed_sample_index(c(1:6))
expect_error(
    get_bootstrap_draws(
        longdata = ld,
        method = method_approxbayes(n_samples = 40, threshold = 0.5),
        use_samp_ids = FALSE,
        ncores = 2
    ),
    regexp = "Try using a simpler covariance"
)
