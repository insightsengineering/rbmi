





test_that("rbmi can handle 'grouped' dplyr data", {

    dat <- simulate_test_data(100) %>%
        as_tibble() %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.2) == 1, NA_real_, outcome))

    dat <- expand_locf(
        dat,
        id = levels(dat$id),
        visit = levels(dat$visit),
        vars = c("age", "group", "sex"),
        group = c("id"),
        order = c("id", "visit")
    )

    dat_ice <- dat %>%
        arrange(id, visit) %>%
        filter(is.na(outcome)) %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup() %>%
        select(id, visit) %>%
        mutate(strategy = "JR")

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        covariates = c("sex*age", "visit*group")
    )

    method <- method_condmean(
        n_samples = 10
    )

    set.seed(987)
    drawObj <- draws(
        data = dplyr::group_by(dat, id),
        data_ice = dplyr::group_by(dat_ice, id),
        vars = vars,
        method = method,
        quiet = TRUE
    )

    imputeObj <- impute(drawObj, references = c("A" = "A", "B" = "A"))

    vars2 <- vars
    vars2$covariates <- c("sex*age")
    anaObj <- analyse(imputeObj, vars = vars2)
    poolObj_1 <- pool(anaObj)



    devnull <- capture.output({
        print(drawObj)
        print(imputeObj)
        print(anaObj)
        print(poolObj_1)
    })
    expect_true(length(devnull) >= 1)


    set.seed(987)
    drawObj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method,
        quiet = TRUE
    )

    imputeObj <- impute(drawObj, references = c("A" = "A", "B" = "A"))

    vars2 <- vars
    vars2$covariates <- c("sex*age")
    anaObj <- analyse(imputeObj, vars = vars2)
    poolObj_2 <- pool(anaObj)

    expect_equal(poolObj_1, poolObj_2)
})




test_that("`rbmi` can handle character data", {
    dat <- simulate_test_data(100) %>%
        as_tibble() %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.2) == 1, NA_real_, outcome)) %>%
        mutate(charvar = sample(c("A", "B", "C"), size = nrow(.), replace = TRUE, prob = c(0.4, 0.2, 0.1)))

    dat2 <- expand_locf(
        dat,
        id = levels(dat$id),
        visit = levels(dat$visit),
        vars = c("age", "group", "sex"),
        group = c("id"),
        order = c("id", "visit")
    )

    dat_ice <- dat2 %>%
        arrange(id, visit) %>%
        filter(is.na(outcome)) %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup() %>%
        select(id, visit) %>%
        mutate(strategy = "JR")

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        covariates = c("sex*age", "visit*group", "charvar*group*age")
    )

    method <- method_condmean(
        n_samples = 2
    )

    set.seed(987)
    drawObj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method,
        quiet = TRUE
    )

    imputeObj <- impute(
        drawObj,
        references = c("A" = "A", "B" = "A")
    )

    expect_s3_class(imputeObj, "imputation")

    vars2 <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        covariates = c("sex*age", "charvar*age")
    )

    anaobj <- analyse(
        imputations = imputeObj,
        vars = vars2
    )
    new_dat <- extract_imputed_dfs(imputeObj, 1)[[1]]
    expect_true(is.factor(new_dat$charvar))
    expect_false(any(is.na(new_dat$charvar)))

    poolobj <- pool(anaobj)
    expect_s3_class(as.data.frame(poolobj), "data.frame")
})
