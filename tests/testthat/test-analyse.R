


suppressPackageStartupMessages({
    library(dplyr)
})


test_that("basic constructions of `analysis` work as expected",{

    x <- as_analysis(
        results = list(
            list(analysis_result(name = 'p1', est = 1)), # A nested structure is necessary here. The top level is a full result list. The 2nd level is each imputation. The 3rd level is each individual analysis result inside each imputation.
            list(analysis_result(name = 'p1', est = 2))
        ),
        method = method_condmean(n_samples = 1)
    )
    expect_true(validate(x))


    x <- as_analysis(
        results = list(
            list(analysis_result(name = 'p1', est = 1)),
            list(analysis_result(name = 'p1', est = 2))
        ),
        method = method_condmean(type = "jackknife")
    )
    expect_true(validate(x))


    x <- as_analysis(
        results = list(
            list(analysis_result(name = 'p1', est = 1, df = 4, se = 1)),
            list(analysis_result(name = 'p1', est = 2, df = 3, se = 3))
        ),
        method = method_bayes(n_samples = 2)
    )
    expect_true(validate(x))


    x <- as_analysis(
        results = list(
            list(analysis_result(name = 'p1', est = 1, df = 4, se = 1)),
            list(analysis_result(name = 'p1', est = 2, df = 3, se = 3))
        ),
        method = method_approxbayes(n_samples = 2)
    )
    expect_true(validate(x))


    x <- as_analysis(
        results = list(
            list(analysis_result(name = 'p1',  est = 1,  df = 4, se = NA)),
            list(analysis_result(name = 'p1',  est = 2,  df = 3, se = NA))
        ),
        method = method_bayes(n_samples = 2)
    )
    expect_true(validate(x))


    x <- as_analysis(
        results = list(
            list(analysis_result(name = 'p1', est = 1, df = 4, se = NA)),
            list(analysis_result(name = 'p1', est = 2,  df = 3, se = NA))
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
                list(analysis_result(name = 'p1', est = 1), analysis_result(name = 'p1', est = 2))
            ),
            method = method_condmean(n_samples = 2)
        ),
        "not equal to nsamp"
    )

    expect_error(
        as_analysis(
            results = list(
                list(analysis_result(name = 'p1', est = 1, df = 4, se = 1)),
                list(analysis_result(name = 'p1', est = 2, df = 3, se = 3))
            ),
            method = method_bayes(n_samples = 3)
        ),
        "not equal to nsamp"
    )

    expect_error(
        as_analysis(
            results = list(
                list(analysis_result(name = 'p1', est = 1, df = 4, se = 1)),
                list(analysis_result(name = 'p1', est = 2, df = 3, se = 3))
            ),
            method = method_approxbayes(n_samples = 3)
        ),
        "not equal to nsamp"
    )


    ### Incorrect analysis parameters
    expect_error(
        as_analysis(
            results = list(
                list(analysis_result(name = 'p1', est1 = 1), analysis_result(name = 'p1', est1 = 2)),
            ),
            method = method_condmean(n_samples = 1)
        ),
        "unused argument \\(est1 = 1\\)"
    )

    expect_error(
        as_analysis(
            results = list(
                list(analysis_result(name = 'p1', est = 1, df1 = 4, se = 1)),
                list(analysis_result(name = 'p1', est = 2, df1 = 3, se = 3))
            ),
            method = method_approxbayes(n_samples = 2)
        ),
        "unused argument \\(df1 = 4\\)"
    )

    expect_error(
        as_analysis(
            results = list(
                list(analysis_result(name = 'p1', est = 1, df1 = 4, se = 1)),
                list(analysis_result(name = 'p1', est = 2, df1 = 3, se = 3))
            ),
            method = method_bayes(n_samples = 2)
        ),
        "unused argument \\(df1 = 4\\)"
    )

    expect_error(
        as_analysis(
            results = list(
                list(analysis_result(name = 'p1',  est = 1, df = 4, se1 = 1)),
                list(analysis_result(name = 'p1',  est = 2, df = 3, se1 = 3))
            ),
            method = method_bayes(n_samples = 2)
        ),
        "unused argument \\(se1 = 1\\)"
    )

    expect_error(
        as_analysis(
            results = list(
                list(analysis_result(name = 'p1', est = 1, df = 4, se1 = 1)),
                list(analysis_result(name = 'p1', est1 = 2, df = 3, se1 = 3))
            ),
            method = method_condmean(type = "jackknife")
        ),
        "unused argument \\(se1 = 1\\)"
    )


    ### Inconsistent analysis parameters  ## Not sure what this is supposed to test
    expect_error(
         as_analysis(
             results = list(
                 list(analysis_result(name = 'p1', est = 1)), # 1st imputation contains one analysis with name "p1"
                 list(analysis_result(name = 'p2', est = 2))  # 2nd imputation contains one analysis with name "p2"
             ),
             method = method_condmean(n_sample = 1)
         ),
         "identically named elements"
     )

    expect_error(
        as_analysis(
            results = list(
                list(list("est" = 1)),
                list(analysis_result(name = 'p1', est = 2))
            ),
            method = method_condmean(n_sample = 1)
        ),
        "Individual analysis result must be type of analysis_result"
    )

    expect_error(
        as_analysis(
            results = list(
                list(list("est" = 1)),
                list(list("est" = 2))
            ),
            method = method_condmean(n_sample = 1)
        ),
        "Individual analysis result must be type of analysis_result"
    )

    ### Invalid values
    expect_error(
        as_analysis(
            results = list(
                list(analysis_result(name = 'p1',  est = NA)),
                list(analysis_result(name = 'p1',  est = 2))
            ),
            method = method_condmean(n_sample = 1)
        ),
        "est of analysis_result is not numeric"
    )

    expect_error(
        as_analysis(
            results = list(
                list(analysis_result(name = 'p1',  est = 'a')),
                list(analysis_result(name = 'p1',  est = 2))
            ),
            method = method_condmean(n_sample = 1)
        ),
        "est of analysis_result is not numeric"
    )

    expect_error(
        as_analysis(
            results = list(
                list(analysis_result(name = 'p1',  est = 1, df = 4, se = NA)),
                list(analysis_result(name = 'p1',  est = 2, df = 3, se = 3))
            ),
            method = method_bayes(n_sample = 2)
        ),
        "`se` contains both missing and observed values"
    )

    x <- as_analysis(
        results = list(
            list(analysis_result(name = 'p1',  est = 1, df = 4, se = NA)),
            list(analysis_result(name = 'p1',  est = 2, df = 3, se = 3))
        ),
        method = method_condmean(n_sample = 1)
    )
    expect_true(validate(x))

})
