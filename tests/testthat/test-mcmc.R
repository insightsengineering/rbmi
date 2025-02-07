suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})


get_mcmc_sim_dat <- function(n, mcoefs, sigma) {
    nv <- ncol(sigma)
    covars <- tibble::tibble(
        id = paste0("P", seq_len(n)),
        age = rnorm(n),
        group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F"))
    )

    dat <- mvtnorm::rmvnorm(n, sigma = sigma) %>%
        set_col_names(paste0("visit_", 1:nv)) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(id = paste0("P", seq_len(n))) %>%
        tidyr::gather("visit", "outcome", -id) %>%
        dplyr::mutate(visit = factor(visit)) %>%
        dplyr::arrange(id, visit) %>%
        dplyr::left_join(covars, by = "id") %>%
        dplyr::mutate(
            outcome = outcome +
                mcoefs[["int"]] +
                mcoefs[["age"]] * age +
                mcoefs[["sex"]] * f2n(sex) +
                mcoefs[["trtslope"]] * f2n(group) * as.numeric(visit)
        ) %>%
        dplyr::mutate(id = as.factor(id))

    return(dat)
}

get_within <- function(x, real) {
    x2 <- matrix(unlist(as.list(x)), nrow = length(x), byrow = TRUE)
    colnames(x2) <- paste0("B", seq_len(ncol(x2)))

    as_tibble(x2) %>%
        tidyr::gather(var, val) %>%
        group_by(var) %>%
        summarise(
            lci = quantile(val, 0.005),
            uci = quantile(val, 0.995)
        ) %>%
        mutate(real = real) %>%
        mutate(inside = real >= lci &  real <= uci)
}

test_extract_draws <- function(draws_extracted, same_cov, n_groups, n_visits) {

    expect_type(draws_extracted, "list")
    expect_length(draws_extracted, 2)
    expect_true(all(names(draws_extracted) %in% c("beta", "sigma")))

    if (same_cov) {
        expect_true(all(sapply(draws_extracted$sigma, function(x) length(x) == 1)))
    } else {
        expect_true(all(sapply(draws_extracted$sigma, function(x) length(x) == n_groups)))
    }

    expect_true(all(sapply(draws_extracted$sigma, function(x) sapply(x, function(y) dim(y) == c(n_visits, n_visits)))))

}


test_that("split_dim creates a list from an array as expected", {
    mat <- rbind(c(1, 0.2), c(0.2, 1))
    a <- array(data = NA, dim = c(3, 2, 2))
    for (i in seq_len(dim(a)[1])) {
        a[i, , ] <- mat + i - 1
    }

    actual_res <- split_dim(a, 1)
    expected_res <- list(mat, mat + 1, mat + 2)
    expect_equal(actual_res, expected_res)
})




test_that("Verbose suppression works", {

    set.seed(301)
    sigma <- as_vcov(c(6, 4, 4), c(0.5, 0.2, 0.3))
    dat <- get_sim_data(50, sigma)

    dat_ice <- dat %>%
        group_by(id) %>%
        arrange(desc(visit)) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(strategy = "MAR")

    vars <- set_vars(
        visit = "visit",
        subjid = "id",
        group = "group",
        covariates = "sex",
        strategy = "strategy",
        outcome = "outcome"
    )

    suppressWarnings({
        msg <- capture.output({
            x <- draws(dat, dat_ice, vars, method_bayes(n_samples = 2), quiet = FALSE)
        })
    })
    expect_true(length(msg) > 0)


    suppressWarnings({
        msg <- capture.output({
            x <- draws(dat, dat_ice, vars, method_bayes(n_samples = 2), quiet = TRUE)
        })
    })
    expect_true(length(msg) == 0)
})




test_that("as_indicies", {

    result_actual <- as_indices(c("1100"))
    result_expected <- list(c(1, 2, 999, 999))
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
        c(1, 3, 5, 999, 999),
        c(1, 2, 4, 999, 999),

        c(999, 999, 999, 999, 999),
        c(1, 2, 3, 4, 5),

        c(2, 3, 4, 5, 999),
        c(1, 2, 3, 4, 999),
        c(1, 3, 4, 5, 999),

        c(1, 999, 999, 999, 999),
        c(2, 999, 999, 999, 999),
        c(5, 999, 999, 999, 999)
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
        n = c(3, 1),
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







test_that("fit_mcmc can recover known values with same_cov = TRUE", {

    skip_if_not(is_full_test())

    set.seed(2151)

    mcoefs <- list(
        "int" = 10,
        "age" = 3,
        "sex" = 6,
        "trtslope" = 7
    )
    sigma <- as_vcov(c(3, 5, 7), c(0.1, 0.4, 0.7))

    dat <- get_mcmc_sim_dat(1000, mcoefs, sigma)
    mat <- model.matrix(data = dat, ~ 1 + sex + age + group + visit + group * visit)

    method <- method_bayes(
        n_samples = 200,
        same_cov = TRUE,
        control = control_bayes(
            warmup = 200,
            thin = 3
        )
    )

    ### No missingness
    fit <- fit_mcmc(
        designmat = mat,
        outcome = dat$outcome,
        group = dat$group,
        subjid = dat$id,
        visit = dat$visit,
        method = method,
        quiet = TRUE
    )

    beta_within <- get_within(fit$samples$beta, c(10, 6, 3, 7, 0, 0, 7, 14))
    assert_that(all(beta_within$inside))

    sigma_within <- get_within(fit$samples$sigma, unlist(as.list(sigma)))
    assert_that(all(sigma_within$inside))

    # check extract_draws() worked properly
    test_extract_draws(
        extract_draws(fit$fit, method$n_samples),
        same_cov = TRUE,
        n_groups = 2,
        n_visits = 3
    )




    ### Random missingness patterns
    set.seed(3190)
    dat2 <- dat %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.3) == 1, NA_real_, outcome))

    fit <- fit_mcmc(
        designmat = mat,
        outcome = dat2$outcome,
        group = dat2$group,
        subjid = dat2$id,
        visit = dat2$visit,
        method = method,
        quiet = TRUE
    )

    beta_within <- get_within(fit$samples$beta, c(10, 6, 3, 7, 0, 0, 7, 14))
    assert_that(all(beta_within$inside))

    sigma_within <- get_within(fit$samples$sigma, unlist(as.list(sigma)))
    assert_that(all(sigma_within$inside))

    # check extract_draws() worked properly
    test_extract_draws(
        extract_draws(fit$fit, method$n_samples),
        same_cov = TRUE,
        n_groups = 2,
        n_visits = 3
    )


    ### Missingness affecting specific groups
    set.seed(3190)
    dat2 <- dat %>%
        mutate(outcome = if_else(
            rbinom(n(), 1, 0.5) == 1 & visit != "visit_1" & group == "B" & age > 0.3,
            NA_real_,
            outcome
        ))

    fit <- fit_mcmc(
        designmat = mat,
        outcome = dat2$outcome,
        group = dat2$group,
        subjid = dat2$id,
        visit = dat2$visit,
        method = method,
        quiet = TRUE
    )

    beta_within <- get_within(fit$samples$beta, c(10, 6, 3, 7, 0, 0, 7, 14))
    assert_that(all(beta_within$inside))

    sigma_within <- get_within(fit$samples$sigma, unlist(as.list(sigma)))
    assert_that(all(sigma_within$inside))

    # check extract_draws() worked properly
    test_extract_draws(
        extract_draws(fit$fit, method$n_samples),
        same_cov = TRUE,
        n_groups = 2,
        n_visits = 3
    )

})


test_that("fit_mcmc returns error if mmrm on original sample fails", {
    set.seed(101)

    mcoefs <- list(
        "int" = 10,
        "age" = 3,
        "sex" = 6,
        "trtslope" = 7
    )
    sigma <- as_vcov(c(3, 5, 7), c(0.1, 0.4, 0.7))

    dat <- get_mcmc_sim_dat(100, mcoefs, sigma)
    mat <- model.matrix(data = dat, ~ 1 + sex + age + group + visit + group * visit)
    mat[, 2] <- 1

    method <- method_bayes(
        n_samples = 2,
        control = control_bayes(thin = 10)
    )

    expect_error(
        fit_mcmc(
            designmat = mat,
            outcome = dat$outcome,
            group = dat$group,
            subjid = dat$id,
            visit = dat$visit,
            method = method,
            quiet = TRUE
        )
        ,
        "Fitting MMRM to original dataset failed"
    )

    method <- method_bayes(
        n_samples = 2,
        same_cov = FALSE,
        control = control_bayes(thin = 10)
    )

    expect_error(
        fit_mcmc(
            designmat = mat,
            outcome = dat$outcome,
            group = dat$group,
            subjid = dat$id,
            visit = dat$visit,
            method = method,
            quiet = TRUE
        )
        ,
        "Fitting MMRM to original dataset failed"
    )
})



test_that("fit_mcmc can recover known values with same_cov = FALSE", {

    skip_if_not(is_full_test())

    set.seed(151)

    mcoefs <- list(
        "int" = 10,
        "age" = 3,
        "sex" = 6,
        "trtslope" = 7
    )
    sigma_a <- as_vcov(c(3, 5, 7), c(0.1, 0.4, 0.7))
    sigma_b <- as_vcov(c(6, 9, 3), c(0.8, 0.2, 0.5))

    dat <- bind_rows(
        get_mcmc_sim_dat(1200, mcoefs, sigma_a) %>%
            filter(group == "A") %>%
            mutate(id = paste0(id, "A")),

        get_mcmc_sim_dat(1200, mcoefs, sigma_b) %>%
            filter(group == "B") %>%
            mutate(id = paste0(id, "B"))
    )

    mat <- model.matrix(data = dat, ~ 1 + sex + age + group + visit + group * visit)

    method <- method_bayes(
        n_samples = 250,
        same_cov = FALSE,
        control = control_bayes(
            warmup = 100,
            thin = 3
        )
    )

    ### No missingness
    fit <- fit_mcmc(
        designmat = mat,
        outcome = dat$outcome,
        group = dat$group,
        subjid = dat$id,
        visit = dat$visit,
        method = method,
        quiet = TRUE
    )

    beta_within <- get_within(fit$samples$beta, c(10, 6, 3, 7, 0, 0, 7, 14))
    assert_that(all(beta_within$inside))

    sig_a <- lapply(fit$samples$sigma, function(x) x[[1]])
    sigma_a_within <- get_within(sig_a, unlist(as.list(sigma_a)))
    assert_that(all(sigma_a_within$inside))

    sig_b <- lapply(fit$samples$sigma, function(x) x[[2]])
    sigma_b_within <- get_within(sig_b, unlist(as.list(sigma_b)))
    assert_that(all(sigma_b_within$inside))

    # check extract_draws() worked properly
    test_extract_draws(
        extract_draws(fit$fit, method$n_samples),
        same_cov = FALSE,
        n_groups = 2,
        n_visits = 3
    )



    ### Random missingness patterns
    set.seed(4812)
    dat2 <- dat %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.2) == 1, NA_real_, outcome))

    fit <- fit_mcmc(
        designmat = mat,
        outcome = dat2$outcome,
        group = dat2$group,
        subjid = dat2$id,
        visit = dat2$visit,
        method = method,
        quiet = TRUE
    )

    beta_within <- get_within(fit$samples$beta, c(10, 6, 3, 7, 0, 0, 7, 14))
    assert_that(all(beta_within$inside))

    sig_a <- lapply(fit$samples$sigma, function(x) x[[1]])
    sigma_a_within <- get_within(sig_a, unlist(as.list(sigma_a)))
    assert_that(all(sigma_a_within$inside))

    sig_b <- lapply(fit$samples$sigma, function(x) x[[2]])
    sigma_b_within <- get_within(sig_b, unlist(as.list(sigma_b)))
    assert_that(all(sigma_b_within$inside))

    # check extract_draws() worked properly
    test_extract_draws(
        extract_draws(fit$fit, method$n_samples),
        same_cov = FALSE,
        n_groups = 2,
        n_visits = 3
    )



})


test_that("burn_in and burn_between arguments to method_bayes are deprecated", {
    expect_error(
        {
            method <- method_bayes(
                n_samples = 250,
                burn_in = 100,
                same_cov = FALSE
            )
        },
        regexp = "burn_in.*deprecated"
    )

    expect_error(
        {
            method <- method_bayes(
                n_samples = 250,
                burn_between = 10,
                same_cov = FALSE
            )
        },
        regexp = "burn_between.*deprecated"
    )
})

test_that("fit_mcmc works with multiple chains", {

    skip_if_not(is_full_test())

    set.seed(3459)

    mcoefs <- list(
        "int" = 10,
        "age" = 3,
        "sex" = 6,
        "trtslope" = 7
    )
    sigma <- as_vcov(c(3, 5, 7), c(0.1, 0.4, 0.7))

    dat <- get_mcmc_sim_dat(1000, mcoefs, sigma)
    mat <- model.matrix(data = dat, ~ 1 + sex + age + group + visit + group * visit)

    method <- method_bayes(
        n_samples = 200,
        same_cov = TRUE,
        control = control_bayes(
            warmup = 200,
            thin = 3,
            chains = 3
        )
    )

    fit <- fit_mcmc(
        designmat = mat,
        outcome = dat$outcome,
        group = dat$group,
        subjid = dat$id,
        visit = dat$visit,
        method = method,
        quiet = TRUE
    )
    expect_true(length(fit$samples$beta) == method$n_samples)
    expect_true(length(fit$samples$sigma) == method$n_samples)

    beta_within <- get_within(fit$samples$beta, c(10, 6, 3, 7, 0, 0, 7, 14))
    assert_that(all(beta_within$inside))

    sigma_within <- get_within(fit$samples$sigma, unlist(as.list(sigma)))
    assert_that(all(sigma_within$inside))

    # check extract_draws() worked properly
    test_extract_draws(
        extract_draws(fit$fit, method$n_samples),
        same_cov = TRUE,
        n_groups = 2,
        n_visits = 3
    )
})
