

suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})


NCORES <- 2


bign <- 700
sigma <- as_vcov(c(2, 1, 0.7), c(0.5, 0.3, 0.2))
nsamp <- 200


expect_pool_est <- function(po, expected, param = "trt_visit_3") {
    expect_contains(
        po$pars[[param]]$ci,
        expected
    )

    expect_contains(
        po$pars[[param]]$ci,
        po$pars[[param]]$est
    )

    if ("lsm_alt_visit_3" %in% names(po$pars)) {
        lsm_trt <- (po$pars$lsm_alt_visit_3$est - po$pars$lsm_ref_visit_3$est)

        expect_within(
            lsm_trt - po$pars[[param]]$est,
            c(-0.005, 0.005)
        )
    }
}



test_that("Basic Usage - Approx Bayes", {

    skip_if_not(is_full_test())

    set.seed(1512)

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
        method = method_approxbayes(n_samples = nsamp),
        quiet = TRUE,
        ncores = NCORES
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

    poolobj <- pool(
        results = anaobj,
        conf.level = 0.99,
        alternative = "two.sided"
    )

    expect_pool_est(poolobj, 4)
})











test_that("Basic Usage - Bayesian", {

    skip_if_not(is_full_test())

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
        method = method_bayes(n_samples = nsamp),
        quiet = TRUE,
        ncores = NCORES
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

    skip_if_not(is_full_test())

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
        method = method_condmean(n_samples = nsamp),
        quiet = TRUE,
        ncores = NCORES
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

    skip_if_not(is_full_test())

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
        method = method_condmean(n_samples = nsamp),
        quiet = TRUE,
        ncores = NCORES
    )


    imputeobj <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B"),
        strategies = getStrategies("XX" = strategy_JR)
    )

    myfun <- function(dat) {
        dat <- dat %>% filter(visit == "visit_3")
        mod <- lm(data = dat, outcome ~ group + age + sex)
        list(
            "treatment_effect" = list(
                "est" = coef(mod)[[2]],
                "se" = sqrt(vcov(mod)[2, 2]),
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

    skip_if_not(is_full_test())

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
        method = method,
        quiet = TRUE,
        ncores = NCORES
    )
    imputeobj <- impute(draws = drawobj, references = c("A" = "B", "B" = "B"))
    anaobj <- analyse(imputeobj, fun = rbmi::ancova, vars = vars2)
    poolobj <- pool(results = anaobj)


    set.seed(984)
    drawobj2 <- draws(
        data = dat2,
        data_ice = dat_ice_2,
        vars = vars,
        method = method,
        quiet = TRUE,
        ncores = NCORES
    )
    imputeobj2 <- impute(draws = drawobj2, references = c("A" = "B", "B" = "B"))
    anaobj2 <- analyse(imputeobj2, fun = rbmi::ancova, vars = vars2)
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

    skip_if_not(is_full_test())

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

    sigma_sd <- c(2, 2, 2)
    sigma_cor <- c(0.1, 0.5, 0.3)

    n <- 100

    dat <- bind_rows(
        simulate_test_data(n, mu = mcoefs_b, sd = sigma_sd, cor = sigma_cor) %>%
            mutate(group2 = if_else(group == "A", "A", "B")) %>%
            mutate(id = paste0(id, "A")),

        simulate_test_data(n, mu = mcoefs_c, sd = sigma_sd, cor = sigma_cor) %>%
            mutate(group2 = if_else(group == "A", "A", "C")) %>%
            mutate(id = paste0(id, "B")),

        simulate_test_data(n, mu = mcoefs_d, sd = sigma_sd, cor = sigma_cor) %>%
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

    set.seed(314)
    x <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_bayes(
            same_cov = FALSE,
            n_samples = 250,
            control = control_bayes(
                warmup = 50,
                thin = 4
            )
        ),
        quiet = TRUE
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
        quiet = TRUE,
        data_ice = dat_ice,
        vars = vars,
        method = method_condmean(
            same_cov = TRUE,
            n_samples = 80
        ),
        ncores = NCORES
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


test_that("rbmi works for one arm trials", {

    skip_if_not(is_full_test())

    # ancova cannot be applied for 1 arm trial. Use a custom analysis function
    myanalysis <- function(data, ...) {

        data_anal <- data[data[[vars$visit]] == "visit_3", ][[vars$outcome]]
        res <- list(
            mean = list(
                est = mean(data_anal),
                se = sd(data_anal) / sqrt(length(data_anal)),
                df = length(data_anal) - 1
            )
        )
        return(res)
    }

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        strata = "group",
        covariates = c("age", "sex", "visit")
    )

    vars_wrong <- vars
    vars_wrong$covariates <- c("age", "sex", "group*visit")

    vars_wrong2 <- vars
    vars_wrong2$covariates <- c("age", "sex", "group", "visit")

    set.seed(169)
    dat_full <- simulate_test_data(n = 100, sd = 0.1 * c(3, 5, 7)) %>% as_tibble()

    ## Introduce missingness
    missing_index_vis2 <- rbinom(nrow(dat_full), 1, 0.3) == 1 & dat_full$visit == "visit_2"
    missing_index_vis3 <- rbinom(nrow(dat_full), 1, 0.4) == 1 & dat_full$visit == "visit_3"

    # select subjects belonging to one group only
    dat <- dat_full %>%
        mutate(outcome = if_else(missing_index_vis2 | missing_index_vis3, NA_real_, outcome)) %>%
        filter(group == "A") %>%
        mutate(group_wrong = group) %>%
        mutate(id = factor(id, levels = unique(id))) %>%
        mutate(group = factor(group, unique(group)))

    vars_wrong3 <- vars
    vars_wrong3$group <- "group_wrong"
    vars_wrong3$strata <- "group_wrong"

    dat_ice <- dat %>%
        arrange(id, visit) %>%
        filter(is.na(outcome)) %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup() %>%
        select(id, visit) %>%
        mutate(strategy = "MAR")

    runtest <- function(dat, dat_ice, vars, vars_wrong, vars_wrong2, vars_wrong3, method) {

        draw_obj <- draws(
            data = dat,
            data_ice = dat_ice,
            vars = vars,
            method = method,
            ncores = NCORES,
            quiet = TRUE
        )

        expect_error(
            draws(
                data = dat,
                data_ice = dat_ice,
                vars = vars_wrong,
                method = method
            ),
            "`group`"
        )

        expect_error(
            draws(
                data = dat,
                data_ice = dat_ice,
                vars = vars_wrong2,
                method = method
            ),
            "`group`"
        )

        expect_error(
            draws(
                data = dat,
                data_ice = dat_ice,
                vars = vars_wrong3,
                method = method
            ),
            "`group_wrong`"
        )

        impute_obj <- impute(
            draw_obj,
            references = c("A" = "A")
        )

        vars$covariates <- c("age", "sex")
        anl_obj <- analyse(
            imputations = impute_obj,
            vars = vars,
            fun = myanalysis
        )

        expect_error(
            analyse(
                imputations = impute_obj,
                vars = vars,
                fun = ancova
            ),
            regexp = "`data[[vars$group]]`",
            fixed = TRUE
        )

        if (class(anl_obj$method)[2] == "condmean") {
            pooled <- pool(anl_obj, type = "normal")
        } else {
            pooled <- pool(anl_obj)
        }

        expect_length(pooled$pars$mean, 4)
        expect_true(all(!is.null(unlist(pooled$pars$mean))))
        expect_true(all(!is.na(unlist(pooled$pars$mean))))
        expect_true(all(is.double(unlist(pooled$pars$mean))))
    }

    method <- method_condmean(type = "jackknife")
    runtest(dat, dat_ice, vars, vars_wrong, vars_wrong2, vars_wrong3, method)

    method <- method_bayes(n_samples = 250, control = control_bayes(thin = 10))
    runtest(dat, dat_ice, vars, vars_wrong, vars_wrong2, vars_wrong3, method)

    method <- method_approxbayes(n_samples = 3)
    runtest(dat, dat_ice, vars, vars_wrong, vars_wrong2, vars_wrong3, method)

    method <- method_condmean(type = "bootstrap", n_samples = 2)
    runtest(dat, dat_ice, vars, vars_wrong, vars_wrong2, vars_wrong3, method)
})


test_that("Three arms trial runs smoothly and gives expected results", {

    copy_group <- function(dat, name_group) {
        datC <- dat[dat$group == name_group, ]
        datC$group <- "C"
        datC$id <- paste0(datC$id, "C")
        dat <- rbind(dat, datC)
        return(dat)
    }

    myanfun <- function(data, ...) {

        # apply ancova between A and B
        data_temp <- data[data$group %in% c("A", "B"), ]
        data_temp$group <- factor(data_temp$group, levels = c("A", "B"))
        resB <- ancova(data_temp, ...)

        # apply ancova between A and C
        data_temp <- data[data$group %in% c("A", "C"), ]
        data_temp$group <- factor(data_temp$group, levels = c("A", "C"))
        resC <- ancova(data_temp, ...)

        ret_obj <- list(
            trtB = resB$trt_visit_3,
            trtC = resC$trt_visit_3
        )

        return(ret_obj)

    }

    set.seed(101)
    bign <- 40
    sigma <- as_vcov(c(2, 1, 0.7), c(0.5, 0.3, 0.2))
    nsamp <- 0

    dat <- get_sim_data(bign, sigma, trt = 8)
    dat <- dat %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss)
    dat <- copy_group(dat, name_group = "B")
    datB <- dat$outcome[dat$group == "B" & !is.na(dat$outcome)]
    datC <- dat$outcome[dat$group == "C" & !is.na(dat$outcome)]
    expect_true(all(datB == datC))

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
        covariates = c("sex", "age", "visit * group")
    )

    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        quiet = TRUE,
        method = method_condmean(n_samples = nsamp, type = "bootstrap", same_cov = TRUE)
    )

    imputeobj <- impute(
        draws = drawobj,
        references = c("A" = "A", "B" = "A", "C" = "A")
    )

    vars2 <- vars
    vars2$covariates <- c("sex", "age")

    anaobj <- analyse(
        imputeobj,
        fun = myanfun,
        vars = vars2,
        visits = "visit_3"
    )
    pooled <- pool(anaobj)

    # check that groups B and C have same imputed values, same estimated and pooled treatment effect
    imp_dat <- extract_imputed_dfs(imputeobj)[[1]]
    expect_equal(imp_dat$outcome[imp_dat$group == "B"], imp_dat$outcome[imp_dat$group == "C"])
    expect_equal(anaobj$results[[1]]$trtB, anaobj$results[[1]]$trtC)
    expect_equal(pooled$pars$trtB, pooled$pars$trtC)


    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        quiet = TRUE,
        method = method_condmean(n_samples = nsamp, type = "bootstrap", same_cov = FALSE)
    )

    imputeobj <- impute(
        draws = drawobj,
        references = c("A" = "A", "B" = "A", "C" = "A")
    )

    anaobj <- analyse(
        imputeobj,
        fun = myanfun,
        vars = vars2,
        visits = "visit_3"
    )
    pooled <- pool(anaobj)

    # check that groups B and C have same imputed values, same estimated and pooled treatment effect
    imp_dat <- extract_imputed_dfs(imputeobj)[[1]]
    expect_equal(imp_dat$outcome[imp_dat$group == "B"], imp_dat$outcome[imp_dat$group == "C"])
    expect_equal(anaobj$results[[1]]$trtB, anaobj$results[[1]]$trtC)
    expect_equal(pooled$pars$trtB, pooled$pars$trtC)
})
