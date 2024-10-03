
suppressPackageStartupMessages({
    library(dplyr)
})


test_that("basic constructions of `analysis` work as expected", {

    oldopt <- getOption("warnPartialMatchDollar")
    options(warnPartialMatchDollar = TRUE)

    x <- as_analysis(
        results = list(
            list(p1 = list("est" = 1)),
            list(p1 = list("est" = 2))
        ),
        method = method_condmean(n_samples = 1)
    )
    expect_true(validate(x))


    x <- as_analysis(
        results = list(
            list(p1 = list("est" = 1)),
            list(p1 = list("est" = 2))
        ),
        method = method_condmean(type = "jackknife")
    )
    expect_true(validate(x))


    x <- as_analysis(
        results = list(
            list(p1 = list("est" = 1, "df" = 4, "se" = 1)),
            list(p1 = list("est" = 2, "df" = 3, "se" = 3))
        ),
        method = method_bayes(n_samples = 2)
    )
    expect_true(validate(x))


    x <- as_analysis(
        results = list(
            list(p1 = list("est" = 1, "df" = 4, "se" = 1)),
            list(p1 = list("est" = 2, "df" = 3, "se" = 3))
        ),
        method = method_approxbayes(n_samples = 2)
    )
    expect_true(validate(x))


    x <- as_analysis(
        results = list(
            list(p1 = list("est" = 1, "df" = 4, "se" = NA)),
            list(p1 = list("est" = 2, "df" = 3, "se" = NA))
        ),
        method = method_bayes(n_samples = 2)
    )
    expect_true(validate(x))


    x <- as_analysis(
        results = list(
            list(p1 = list("est" = 1, "df" = 4, "se" = NA)),
            list(p1 = list("est" = 2, "df" = 3, "se" = NA))
        ),
        method = method_approxbayes(n_samples = 2)
    )
    expect_true(validate(x))

    options(warnPartialMatchDollar = oldopt)
})


test_that("incorrect constructions of as_analysis fail", {

    #### Incorrect number of samples
    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = 1)),
                list(p1 = list("est" = 2))
            ),
            method = method_condmean(n_samples = 2)
        ),
        "not equal to nsamp"
    )

    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = 1, "df" = 4, "se" = 1)),
                list(p1 = list("est" = 2, "df" = 3, "se" = 3))
            ),
            method = method_bayes(n_samples = 3)
        ),
        "not equal to nsamp"
    )

    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = 1, "df" = 4, "se" = 1)),
                list(p1 = list("est" = 2, "df" = 3, "se" = 3))
            ),
            method = method_approxbayes(n_samples = 3)
        ),
        "not equal to nsamp"
    )


    ### Incorrect analysis parameters
    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est1" = 1)),
                list(p1 = list("est1" = 2))
            ),
            method = method_condmean(n_samples = 1)
        ),
        "`est`"
    )

    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = 1, "df1" = 4, "se" = 1)),
                list(p1 = list("est" = 2, "df1" = 3, "se" = 3))
            ),
            method = method_approxbayes(n_samples = 2)
        ),
        "`df`"
    )

    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = 1, "df1" = 4, "se" = 1)),
                list(p1 = list("est" = 2, "df1" = 3, "se" = 3))
            ),
            method = method_bayes(n_samples = 2)
        ),
        "`df`"
    )

    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = 1, "df" = 4, "se1" = 1)),
                list(p1 = list("est" = 2, "df" = 3, "se1" = 3))
            ),
            method = method_bayes(n_samples = 2)
        ),
        "`se`"
    )

    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = 1, "df" = 4, "se1" = 1)),
                list(p1 = list("est1" = 2, "df" = 3, "se1" = 3))
            ),
            method = method_condmean(type = "jackknife")
        ),
        "`est`"
    )


    ### Inconsistent analysis parameters
    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = 1)),
                list(p2 = list("est" = 2))
            ),
            method = method_condmean(n_sample = 1)
        ),
        "identically named elements"
    )

    expect_error(
        as_analysis(
            results = list(
                list(list("est" = 1)),
                list(p1 = list("est" = 2))
            ),
            method = method_condmean(n_sample = 1)
        ),
        "results must be named lists"
    )

    expect_error(
        as_analysis(
            results = list(
                list(list("est" = 1)),
                list(list("est" = 2))
            ),
            method = method_condmean(n_sample = 1)
        ),
        "results must be named lists"
    )

    ### Invalid values
    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = NA)),
                list(p1 = list("est" = 2))
            ),
            method = method_condmean(n_sample = 1)
        ),
        "`est` contains missing values"
    )

    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = "a")),
                list(p1 = list("est" = 2))
            ),
            method = method_condmean(n_sample = 1)
        ),
        "result is type 'character'"
    )

    expect_error(
        as_analysis(
            results = list(
                list(p1 = list("est" = 1, "df" = 4, "se" = NA)),
                list(p1 = list("est" = 2, "df" = 3, "se" = 3))
            ),
            method = method_bayes(n_sample = 2)
        ),
        "`se` contains both missing and observed values"
    )

    x <- as_analysis(
        results = list(
            list(p1 = list("est" = 1, "df" = 4, "se" = NA)),
            list(p1 = list("est" = 2, "df" = 3, "se" = 3))
        ),
        method = method_condmean(n_sample = 1)
    )
    expect_true(validate(x))

})





test_that("Parallisation works with analyse and produces identical results", {
    set.seed(4642)
    sigma <- as_vcov(
        c(2, 1, 0.7, 1.5),
        c(0.5, 0.3, 0.2, 0.3, 0.5, 0.4)
    )
    dat <- get_sim_data(200, sigma, trt = 8) %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.3) == 1 & group == "A", NA_real_, outcome))

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

    set.seed(984)
    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_condmean(n_samples = 6, type = "bootstrap"),
        quiet = TRUE
    )

    imputeobj <- impute(
        draws = drawobj,
        references = c("A" = "B", "B" = "B")
    )


    #
    # Here we set up a bunch of different analysis objects using different
    # of parallelisation methods and different dat_delta objects
    #


    ### Delta 1

    dat_delta_1 <- delta_template(imputations = imputeobj) %>%
        mutate(delta = is_missing * 5)

    vars2 <- vars
    vars2$covariates <- c("age", "sex")

    anaobj_d1_t1 <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2,
        delta = dat_delta_1
    )

    anaobj_d1_t2 <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2,
        delta = dat_delta_1,
        ncores = 3
    )

    var <- 20
    inner_fun <- function(...) {
        x <- day(var) # lubridate::day
        rbmi::ancova(...)
    }
    outer_fun <- function(...) {
        inner_fun(...)
    }

    cl <- make_rbmi_cluster(3, objects = list(var = var, inner_fun = inner_fun), "lubridate")
    anaobj_d1_t3 <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2,
        delta = dat_delta_1,
        ncores = cl
    )


    ### Delta 2

    dat_delta_2 <- delta_template(imputations = imputeobj) %>%
        mutate(delta = is_missing * 50)

    anaobj_d2_t1 <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2,
        delta = dat_delta_2
    )
    anaobj_d2_t3 <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2,
        delta = dat_delta_2,
        ncores = cl
    )


    ### Delta 3 (no delta)

    anaobj_d3_t1 <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2
    )
    anaobj_d3_t3 <- analyse(
        imputeobj,
        fun = rbmi::ancova,
        vars = vars2,
        ncores = cl
    )

    ## Check for internal consistency
    expect_equal(anaobj_d1_t1, anaobj_d1_t2)
    expect_equal(anaobj_d1_t1, anaobj_d1_t3)
    expect_equal(anaobj_d1_t2, anaobj_d1_t3)

    expect_equal(anaobj_d2_t1, anaobj_d2_t3)

    expect_equal(anaobj_d3_t1, anaobj_d3_t3)


    ## Check that they differ (as different deltas have been used)
    ## Main thing is sanity checking that the embedded delta
    ## in the parallel processes hasn't lingered and impacted
    ## future results

    # First assert consistency
    expect_true(identical(anaobj_d1_t1$results, anaobj_d1_t3$results))
    expect_true(identical(anaobj_d2_t1$results, anaobj_d2_t3$results))
    expect_true(identical(anaobj_d3_t1$results, anaobj_d3_t3$results))

    # The ensure they are different
    expect_false(identical(anaobj_d1_t1$results, anaobj_d2_t1$results))
    expect_false(identical(anaobj_d1_t1$results, anaobj_d3_t1$results))
    expect_false(identical(anaobj_d2_t1$results, anaobj_d3_t1$results))

})
