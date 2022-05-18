# Test functions relavent to analysis_result class

test_that("basic constructions of `analysis_result` work as expected", {

    expect_general <- function(x) {
        expect_s3_class(x, c('analysis_result', 'list'))
        expect_type(x, 'list')
        expect_named(x)
    }

    # with optional elements: df, meta
    x <- analysis_result(name = 'trt',
                         est = 1,
                         se = 2,
                         df = 3,
                         meta = list(visit = 1))
    expect_general(x)
    expect_length(x, 5)
    expect_equal(names(x), c('name', 'est', 'se', 'df', 'meta'))
    expect_true(assertthat::has_attr(x, 'meta'))
    expect_equal(x$name, 'trt')
    expect_equal(x$df, 3)
    expect_equal(x$meta, list(visit = 1))

    # without optional elements
    x <- analysis_result(name = 'trt',
                         est = 1,
                         se = 2,
                         meta = list(visit = 1))
    expect_general(x)
    expect_length(x, 4)
    expect_equal(names(x), c('name', 'est', 'se', 'meta'))
    expect_true(has_attr(x, 'meta'))
    expect_equal(x$name, 'trt')
    expect_equal(x$meta, list(visit = 1))

    # without optional elements
    x <- analysis_result(name = 'trt',
                         est = 1)
    expect_general(x)
    expect_length(x, 2)
    expect_equal(names(x), c('name', 'est'))
    expect_false(has_attr(x, 'meta'))
    expect_equal(x$name, 'trt')
    expect_equal(x$est, 1)

    # special input: se = NA
    x <- analysis_result(name = 'trt',
                         est = 1,
                         se = NA)
    expect_general(x)

    expect_length(x, 3)
    expect_equal(names(x), c('name', 'est', 'se'))
    expect_true(is.na(x$se))

    # special input: df = NA
    x <- analysis_result(name = 'trt',
                         est = 1,
                         se = 2,
                         df = NA)
    expect_general(x)
    expect_length(x, 4)
    expect_equal(names(x), c('name', 'est', 'se', 'df'))
    expect_true(is.na(x$df))
})


test_that("incorrect constructions of analysis_result fail", {

    # test parameter type
    expect_error(
        analysis_result(name = 1,
                        est = 1,
                        se = 2,
                        df = 3,
                        meta = list(visit = 1)),
        "name of analysis_result is not character"
    )

    expect_error(
        analysis_result(name = 'a',
                        est = 'b',
                        se = 2,
                        df = 3,
                        meta = list(visit = 1)),
        "est of analysis_result is not numeric"
    )

    expect_error(
        analysis_result(name = 'a',
                        est = 1,
                        se = list(),
                        df = 3,
                        meta = list(visit = 1)),
        "se of analysis_result is not numeric or NA or NULL"
    )

    expect_error(
        analysis_result(name = 'a',
                        est = 1,
                        se = 2,
                        df = data.frame(),
                        meta = list(visit = 1)),
        "df of analysis_result is not numeric or NA or NULL"
    )

    expect_error(
        analysis_result(name = 'a',
                        est = 1,
                        se = 2,
                        df = 3,
                        meta = 'b'),
        "meta of analysis_result is not list or NULL"
    )

    # test parameter length
    expect_error(
        analysis_result(name = c('a', 'b'),
                        est = 1,
                        se = 2,
                        df = 3,
                        meta = list(visit = 1)),
        "length of name in analysis_result `2` is not 1"
    )

    expect_error(
        analysis_result(name = 'a',
                        est = c(1,2,3),
                        se = 2,
                        df = 3,
                        meta = list(visit = 1)),
        "length of est in analysis_result `3` is not 1"
    )

    expect_error(
        analysis_result(name = 'a',
                        est = 1,
                        se = c(1,2,3,4),
                        df = 3,
                        meta = list(visit = 1)),
        "length of se in analysis_result `4` is not 1"
    )

    expect_error(
        analysis_result(name = 'a',
                        est = 1,
                        se = 2,
                        df = c(1,2,3,4,5),
                        meta = list(visit = 1)),
        "length of df in analysis_result `5` is not 1"
    )
})

# Test for as_analysis_result
# This test needs to be updated accordingly if ana_name_chker has been udpated
test_that("as_analysis_result works as expected", {
    expect_general <- function(x) {
        expect_s3_class(x, c("analysis_result", "list"))
        expect_equal(typeof(x), "list")
        expect_equal(x$name, 'a')
        expect_equal(x$est, 1)
        expect_equal(x$se, 2)
    }

    x <- as_analysis_result(list(name='a', est=1, se = 2))
    expect_general(x)

    x <- as_analysis_result(list(name='a', est=1, se = 2, meta = list(visit = 1)))
    expect_general(x)
    expect_true(has_attr(x, 'meta'))

    # Test error input
    expect_error(as_analysis_result(list(name='a')))
    expect_false('met' %in% names(as_analysis_result(list(name='a', est=1, se = 2, met = list(visit = 1)))))
    }
)


test_that("ana_name_chker works as expected", {
    f <- ana_name_chker()
    expect_equal(class(f), "function")
    expect_equal(class(f('musthave_in_objnames')), 'function')
    expect_equal(class(f('objnames_in_musthave')), 'function')
    expect_equal(typeof(f('musthave')), 'character')
    expect_equal(typeof(f('optional')), 'character')
})

test_that("is.analysis_result works as expected", {
    x <- analysis_result(name = 'trt',
                         est = 1,
                         se = 2,
                         df = 3,
                         meta = list(visit = 1))
    expect_true(is.analysis_result(x))
    expect_false(is.analysis_result(list(a=1)))
    expect_false(is.analysis_result(structure(list(a=1), class=c('analyis_result', 'list'))))
})

test_that("analysis_info works as expected", {

    # check for normal input
    test_names <- c('a', 'b', 'c', 'd', 'e', 'f', 'g')
    test_ests <- seq(length(test_names))
    test_ses <- test_ests * 0.1
    test_dfs<- test_ests * 5
    test_metas <- lapply(test_ests, function(x) list(visit=x))

    tab <- mapply(function(a,b,c,d,e) analysis_result(name=a, est=b, se=c, df=d, meta=e),
                  test_names, test_ests, test_ses, test_dfs, test_metas, SIMPLIFY = FALSE)

    x <- analysis_info(tab)

    expect_equal(class(x), "data.frame")
    expect_equal(nrow(x), 7)
    expect_equal(ncol(x), 5)


    # check for NA and NULL input
    test_ses[[2]] <- NA
    test_dfs[[4]] <- NA

    tab <- mapply(function(a,b,c,d,e) analysis_result(name=a, est=b, se=c, df=d, meta=e),
                  test_names, test_ests, test_ses, test_dfs, test_metas, SIMPLIFY = FALSE)
    tab[[1]] <- tab[[1]][-5]
    tab[[1]] <- as_analysis_result(tab[[1]])
    x <- analysis_info(tab)
    expect_true(is.na(x[[2, 3]]))
    expect_true(is.na(x[[4, 4]]))
    expect_true(is.na(x[[1, 5]]))
})
