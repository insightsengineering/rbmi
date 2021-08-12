

suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})



expect_within <- function(x, bounds) {
    expect_gt(x, bounds[1])
    expect_lt(x, bounds[2])
}

expect_contains <- function(x, y){
    expect_within(y, x)
}

expect_pool_est <- function(pool, expected, margin = 0.2){
    expect_within(
        pool$pars$trt$est,
        c(expected - margin, expected + margin)
    )

    lsm_trt <- (pool$pars$lsm_1$est - pool$pars$lsm_0$est)

    expect_within(
        lsm_trt - pool$pars$trt$est,
        c(-0.005, 0.005)
    )
}




test_that("Basic Usage - Approx Bayes", {
    sigma <- as_covmat(c(3,4,5), c(0.8, 0.6, 0.4))

    set.seed(2131)

    dat <- get_sim_data(600, sigma, trt = 8) %>%
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
        mutate(method = "JR")


    vars <- list(
        outcome = "outcome",
        group = "group",
        method = "method",
        subjid = "id",
        visit = "visit",
        covariates = c("age", "sex", "visit * group")
    )

    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_approxbayes(n_samples = 200)
    )

    imputeobj <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B")
    )

    vars2 <- vars
    vars2$covariates <- c("age", "sex")

    anaobj <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2,
        visit_level = "visit_3"
    )

    poolobj <- pool(
        results = anaobj,
        conf.level = 0.95,
        alternative = "two.sided"
    )

    expect_pool_est(poolobj, 4)

})











test_that("Basic Usage - Bayesian", {

    sigma <- as_covmat(c(3,2,1), c(0.8, 0.3, 0.1))

    set.seed(2111)

    dat <- get_sim_data(600, sigma, trt = 8) %>%
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
        mutate(method = "MAR")


    vars <- list(
        outcome = "outcome",
        group = "group",
        method = "method",
        subjid = "id",
        visit = "visit",
        covariates = c("age", "sex", "visit * group")
    )

    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_bayes(n_samples = 250)
    )

    ### Check to see if updating ice works and if it impacts the original values
    updated_ice <- dat_ice %>%
        mutate(method = "JR")

    imputeobj_upd <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B"),
        update_ice = updated_ice
    )

    imputeobj <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B")
    )

    vars2 <- vars
    vars2$covariates <- c("age", "sex")

    anaobj <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2,
        visit_level = "visit_3"
    )

    anaobj_upd <- analyse(
        imputeobj_upd,
        fun = rbmi::ancova,
        vars = vars2,
        visit_level = "visit_3"
    )

    poolobj_upd <- pool(anaobj_upd)

    poolobj <- pool(
        results = anaobj,
        conf.level = 0.95,
        alternative = "two.sided"
    )

    expect_pool_est(poolobj_upd, 4)
    expect_pool_est(poolobj, 8)

})








test_that("Basic Usage - Condmean", {

    sigma <- as_covmat(c(3,4,1), c(0.8, 0.3, 0.1))

    set.seed(21)

    dat <- get_sim_data(700, sigma, trt = 8) %>%
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
        mutate(method = "JR")


    vars <- list(
        outcome = "outcome",
        group = "group",
        method = "method",
        subjid = "id",
        visit = "visit",
        covariates = c("age", "sex", "visit * group")
    )

    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_condmean(n_samples = 250)
    )

    ### Check to see if updating ice works and if it impacts the original values
    updated_ice <- dat_ice %>%
        mutate(method = "MAR")

    imputeobj_upd <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B"),
        update_ice = updated_ice
    )

    imputeobj <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B")
    )

    vars2 <- vars
    vars2$covariates <- c("age", "sex")

    anaobj <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2,
        visit_level = "visit_3"
    )

    anaobj_upd <- analyse(
        imputeobj_upd,
        fun = rbmi::ancova,
        vars = vars2,
        visit_level = "visit_3"
    )

    poolobj_upd <- pool(anaobj_upd)

    poolobj <- pool(
        results = anaobj,
        conf.level = 0.95,
        alternative = "two.sided"
    )

    expect_pool_est(poolobj_upd, 8)
    expect_pool_est(poolobj, 4)


})








test_that("Custom Strategies and Custom analysis functions",{
    sigma <- as_covmat(c(3,2,0.5), c(0.5, 0.3, 0.2))

    set.seed(52131)

    dat <- get_sim_data(800, sigma, trt = 8) %>%
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
        mutate(method = "XX")


    vars <- list(
        outcome = "outcome",
        group = "group",
        method = "method",
        subjid = "id",
        visit = "visit",
        covariates = c("age", "sex", "visit * group")
    )


    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_condmean(n_samples = 250)
    )


    imputeobj <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B"),
        strategies = getStrategies("XX" = strategy_JR)
    )

    myfun <- function(dat){
        dat <- dat %>% filter(visit == "visit_3")
        mod <- lm(data = dat, outcome ~ group + age + sex)
        list(
            "treatment_effect" = list(
                "est" = coef(mod)[[2]],
                "se" = sqrt(vcov(mod)[2,2]),
                "df" = df.residual(mod)
            )
        )
    }

    anaobj <- analyse(
        imputeobj,
        fun = myfun
    )

    poolobj <- pool(anaobj)

    expect_within(
        poolobj$pars$treatment_effect$est,
        4 + c(-0.3, 0.3)
    )



    dat_delta <- delta_template(imputeobj) %>%
        as_tibble() %>%
        mutate(delta = if_else(group == "B" & is_missing, 20, 0))

    anaobj_delta <- analyse(
        imputeobj,
        fun = myfun,
        delta = dat_delta
    )

    poolobj_delta <- pool(anaobj_delta)

    expect_contains(
        poolobj_delta$pars$treatment_effect$ci,
        14
    )



    dat_delta <- delta_template(imputeobj) %>%
        as_tibble() %>%
        mutate(delta = if_else(group == "B" & is_missing, 40, 0))


    anaobj_delta <- analyse(
        imputeobj,
        fun = myfun,
        delta = dat_delta
    )

    poolobj_delta <- pool(anaobj_delta)

    expect_contains(
        poolobj_delta$pars$treatment_effect$ci,
        24
    )



    ddat <- tibble(
        id = "1",
        visit = "visit_2",
        delta = 40
    )

    exdat1 <- extract_imputed_dfs(imputeobj, c(1, 2, 3), idmap = TRUE)
    exdat2 <- extract_imputed_dfs(imputeobj, c(2, 2, 1), idmap = TRUE)
    exdat3 <- extract_imputed_dfs(imputeobj, 2, ddat, idmap = TRUE)
 
    expect_equal(exdat1[1], exdat2[3])
    expect_equal(select(exdat1[[2]], -outcome), select(exdat3[[1]], -outcome))

    # The number of places outcome should differ between exdat1 and exdat3 is equal
    # to the number of times that subject "1" was chosen to be in the sample
    expect_equal(
        nrow(exdat1[[2]]) - sum(exdat1[[2]]$outcome == exdat3[[1]]$outcome),
        sum(attr(exdat1[[2]], "idmap") == "1")
    )
})

