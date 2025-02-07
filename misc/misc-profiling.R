


devtools::load_all()
library(dplyr)
library(testthat)
library(tibble)

sigma <- as_vcov(c(2, 1, 0.7,3,0.1), c(0.5, 0.3, 0.2, 0.1, 0.2, 0.3, 0.5, 0.6,0.1, 0.3))
nsamp <- 500





set.seed(1512)

dat <- get_sim_data(nsamp, sigma, trt = 8) %>%
    mutate(is_miss = rbinom(n(), 1, 0.4)) %>%
    mutate(outcome = if_else(is_miss == 1, NA_real_, outcome)) %>%
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
# profvis::profvis({
    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_bmlmi(B = 30, D = 10)
    )
# })

# 84 seconds

profvis::profvis({
    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_approxbayes(n_samples = 100)
    )
})





# 3 seconds

profvis::profvis({
    imputeobj <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B")
    )
})

# 1.5 seconds

profvis::profvis({
    vars2 <- vars
    vars2$covariates <- c("age", "sex")

    anaobj <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2
    )
})


poolobj <- pool(
    results = anaobj,
    conf.level = 0.99,
    alternative = "two.sided"
)

expect_pool_est(poolobj, 4)











test_that("Basic Usage - Bayesian", {

    skip_if_not(is_nightly())

    set.seed(5123)

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
        mutate(strategy = "MAR")


    vars <- set_vars(
        outcome = "outcome",
        group = "group",
        strategy = "strategy",
        subjid = "id",
        visit = "visit",
        covariates = c("age", "sex", "visit * group")
    )

    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_bayes(n_samples = nsamp)
    )

    ### Check to see if updating ice works and if it impacts the original values
    updated_ice <- dat_ice %>%
        mutate(strategy = "JR")

    imputeobj_upd <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B"),
        update_strategy = updated_ice
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
        visits = "visit_3"
    )

    anaobj_upd <- analyse(
        imputeobj_upd,
        fun = rbmi::ancova,
        vars = vars2,
        visits = "visit_3"
    )

    poolobj_upd <- pool(
        results = anaobj_upd,
        conf.level = 0.99,
        alternative = "two.sided"
    )

    poolobj <- pool(
        results = anaobj,
        conf.level = 0.99,
        alternative = "two.sided"
    )

    expect_pool_est(poolobj_upd, 4)
    expect_pool_est(poolobj, 8)
})





test_that("Basic Usage - Condmean", {

    skip_if_not(is_nightly())

    set.seed(4642)

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

    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_condmean(n_samples = nsamp)
    )

    ### Check to see if updating ice works and if it impacts the original values
    updated_ice <- dat_ice %>%
        mutate(strategy = "MAR")

    imputeobj_upd <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B"),
        update_strategy = updated_ice
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
        visits = "visit_3"
    )

    anaobj_upd <- analyse(
        imputeobj_upd,
        fun = rbmi::ancova,
        vars = vars2,
        visits = "visit_3"
    )

    poolobj_upd <- pool(anaobj_upd)

    poolobj <- pool(
        results = anaobj,
        conf.level = 0.99,
        alternative = "two.sided"
    )

    expect_pool_est(poolobj_upd, 8)
    expect_pool_est(poolobj, 4)
})








test_that("Custom Strategies and Custom analysis functions", {

    skip_if_not(is_nightly())

    set.seed(8368)

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
        mutate(strategy = "XX")


    vars <- set_vars(
        outcome = "outcome",
        group = "group",
        strategy = "strategy",
        subjid = "id",
        visit = "visit",
        covariates = c("age", "sex", "visit * group")
    )


    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_condmean(n_samples = nsamp)
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

    expect_pool_est(poolobj_delta, 14, "treatment_effect")



    dat_delta <- delta_template(imputeobj) %>%
        as_tibble() %>%
        mutate(delta = if_else(group == "B" & is_missing, 40, 0))


    anaobj_delta <- analyse(
        imputeobj,
        fun = myfun,
        delta = dat_delta
    )

    poolobj_delta <- pool(anaobj_delta)

    expect_pool_est(poolobj_delta, 24, "treatment_effect")




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



test_that("Sorting doesn't change results", {

    skip_if_not(is_nightly())

    set.seed(4642)

    dat <- get_sim_data(100, sigma, trt = 8) %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.3) == 1, NA_real_, outcome))

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

    vars2 <- vars
    vars2$covariates <- c("age", "sex")

    dat2 <- dat %>% sample_frac(1)
    dat_ice_2 <- dat_ice %>% sample_frac(1)

    expect_equal(
        dat %>% arrange(id, visit),
        dat2 %>% arrange(id, visit)
    )
    expect_equal(
        dat_ice %>% arrange(id, visit),
        dat_ice_2 %>% arrange(id, visit)
    )

    method <- method_condmean(n_samples = 10)

    set.seed(984)
    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method
    )
    imputeobj <- impute( draws = drawobj, references = c("A" = "B", "B" = "B"))
    anaobj <- analyse( imputeobj, fun = rbmi::ancova, vars = vars2)
    poolobj <- pool(results = anaobj)


    set.seed(984)
    drawobj2 <- draws(
        data = dat2,
        data_ice = dat_ice_2,
        vars = vars,
        method = method
    )
    imputeobj2 <- impute( draws = drawobj2, references = c("A" = "B", "B" = "B"))
    anaobj2 <- analyse( imputeobj2, fun = rbmi::ancova, vars = vars2)
    poolobj2 <- pool(results = anaobj2)

    ## Tidy up things that will never be the same:
    drawobj$formula <- NULL
    drawobj2$formula <- NULL
    anaobj$call <- NULL
    anaobj2$call <- NULL

    expect_equal(drawobj, drawobj2)
    expect_equal(imputeobj, imputeobj2)
    expect_equal(anaobj, anaobj2)
    expect_equal(poolobj, poolobj2)
})







test_that("Multiple imputation references / groups work as expected (end to end checks)", {

    skip_if_not(is_nightly())

    extract_ci <- function(x, ref) {
        x_imp <- impute(x, ref)
        vars2 <- x$data$vars
        vars2$covariates <- c("age", "sex")
        vars2$group <- "group"
        x_ana <- analyse(x_imp, ancova, vars = vars2)
        x_pl <- pool(x_ana, conf.level = 0.98)
        x_pl$pars$trt_visit_3$ci
    }

    set.seed(2351)

    s1 <- 6
    s2 <- 9
    s3 <- 28

    mcoefs_b <- list("int" = 10, "age" = 3, "sex" = 6, "trt" = s1, "visit" = 2)
    mcoefs_c <- list("int" = 10, "age" = 3, "sex" = 6, "trt" = s2, "visit" = 2)
    mcoefs_d <- list("int" = 10, "age" = 3, "sex" = 6, "trt" = s3, "visit" = 2)

    sigma_sd <-c(2, 2, 2)
    sigma_cor <- c(0.1, 0.5, 0.3)

    n <- 100

    dat <- bind_rows(
        simulate_data(n, mu = mcoefs_b, sd = sigma_sd, cor = sigma_cor) %>%
            mutate(group2 = if_else(group == "A", "A", "B")) %>%
            mutate(id = paste0(id, "A")),

        simulate_data(n, mu = mcoefs_c, sd = sigma_sd, cor = sigma_cor) %>%
            mutate(group2 = if_else(group == "A", "A", "C")) %>%
            mutate(id = paste0(id, "B")),

        simulate_data(n, mu = mcoefs_d, sd = sigma_sd, cor = sigma_cor) %>%
            mutate(group2 = if_else(group == "A", "A", "D")) %>%
            mutate(id = paste0(id, "C"))
    ) %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.5) == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        mutate(id = factor(id)) %>%
        mutate(group2 = factor(group2, levels = c("A", "B", "C", "D")))

    vars <- set_vars(
        subjid = "id",
        outcome = "outcome",
        visit = "visit",
        group = "group2",
        covariates = c("age", "sex", "group2 * visit")
    )

    dat_ice <- dat %>%
        filter(is.na(outcome)) %>%
        group_by(id) %>%
        arrange(id, visit) %>%
        slice(1) %>%
        select(id, visit) %>%
        mutate(strategy = "JR")

    x <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_bayes(
            same_cov = FALSE,            
            n_samples = 150,
            control = control_bayes(
                warmup = 25,
                thin = 4
            )
        ),
        quiet = TRUE
    )


    lm(
        data = dat %>% filter(visit == "visit_3"),
        outcome ~ sex + age + group
    )



    t1 <- (s1 + 0) / 2
    t2 <- (s2 + 0) / 2
    t3 <- (s3 + 0) / 2
    expect_within(
        mean(c(t1, t2, t3)),
        extract_ci(x, c("A" = "A", "B" = "A", "C" = "A", "D" = "A"))
    )

    t1 <- (s1 + s1) / 2
    t2 <- (s2 + s2) / 2
    t3 <- (s3 + s3) / 2
    expect_within(
        mean(c(t1, t2, t3)),
        extract_ci(x, c("A" = "A", "B" = "B", "C" = "C", "D" = "D"))
    )

    t1 <- (s1 + s2) / 2
    t2 <- (s2 + s2) / 2
    t3 <- (s3 + s2) / 2
    expect_within(
        mean(c(t1, t2, t3)),
        extract_ci(x, c("A" = "A", "B" = "C", "C" = "C", "D" = "C"))
    )


    x <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_condmean(
            same_cov = TRUE,
            n_samples = 60
        )
    )

    expect_true(
        length(x$samples[[1]]$sigma) == 4
    )

    expect_equal(
        x$samples[[1]]$sigma[["A"]],
        x$samples[[1]]$sigma[["D"]]
    )

    expect_true(
        !identical(x$samples[[1]]$sigma[["A"]], x$samples[[2]]$sigma[["A"]])
    )

    t1 <- (s1 + 0) / 2
    t2 <- (s2 + 0) / 2
    t3 <- (s3 + 0) / 2
    expect_within(
        mean(c(t1, t2, t3)),
        extract_ci(x, c("A" = "A", "B" = "A", "C" = "A", "D" = "A"))
    )

    t1 <- (s1 + s1) / 2
    t2 <- (s2 + s2) / 2
    t3 <- (s3 + s3) / 2
    expect_within(
        mean(c(t1, t2, t3)),
        extract_ci(x, c("A" = "A", "B" = "B", "C" = "C", "D" = "D"))
    )

    t1 <- (s1 + s2) / 2
    t2 <- (s2 + s2) / 2
    t3 <- (s3 + s2) / 2
    expect_within(
        mean(c(t1, t2, t3)),
        extract_ci(x, c("A" = "A", "B" = "C", "C" = "C", "D" = "C"))
    )
})

