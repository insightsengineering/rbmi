


suppressPackageStartupMessages({
    library(dplyr)
})


test_that("basic constructions of `analysis` work as expected",{

    expect_true(FALSE)

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
        "`se` contains missing values"
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
