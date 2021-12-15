





test_that("rbmi can handle 'grouped' dplyr data", {

    dat <- simulate_data(100) %>%
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
        method = method
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
        method = method
    )

    imputeObj <- impute(drawObj, references = c("A" = "A", "B" = "A"))

    vars2 <- vars
    vars2$covariates <- c("sex*age")
    anaObj <- analyse(imputeObj, vars = vars2)
    poolObj_2 <- pool(anaObj)

    expect_equal(poolObj_1, poolObj_2)
})





