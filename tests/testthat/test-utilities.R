suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})


test_that("as_model_df", {

    x <- tibble(
        x = c(NA, 1, 2, 3),
        y = c(5, 6, 7, 8),
        z = c("A", "B", "C", "A"),
        w = c("A", "A", "B", "B")
    )

    actual_output <- as_model_df(x, x ~ y + z * w)

    expected_output <- tibble(
        outcome = c(NA, 1, 2, 3),
        V1 = c(1, 1, 1, 1),
        V2 = c(5, 6, 7, 8),
        V3 = c(0, 1, 0, 0),
        V4 = c(0, 0, 1, 0),
        V5 = c(0, 0, 1, 1),
        V6 = c(0, 0, 0, 0),
        V7 = c(0, 0, 1, 0)
    )
    rownames(expected_output) <- NULL
    rownames(actual_output) <- NULL

    expect_equal(actual_output, expected_output)

    i2 <- iris
    i2[c(1, 2, 3), "Sepal.Length"] <- NA
    i2[c(4, 5, 6), "Sepal.Length"] <- Inf
    x <- as_model_df(i2, Sepal.Length ~ Sepal.Width * Species)
    expect_true(nrow(x) == nrow(i2))

    i3 <- i2
    i3["Sepal.Width", c(1, 2, 3)] <- NA
    expect_error(
        as_model_df(i3, Sepal.Length ~ Sepal.Width * Species),
        regexp = "You may have missing values"
    )
})


test_that("as_model_df fails if formula has one factor variable", {
    cov1 <- rep("A", 10)
    outcome <- rnorm(10)
    dat <- data.frame(outcome = outcome, cov1 = cov1)
    frm <- outcome ~ cov1
    expect_error(as_model_df(dat = dat, frm = frm))
})


test_that("as_simple_formula", {

    vars <- list(
        outcome = "outcome",
        group = "group",
        visit = "visit"
    )

    actual <- as_simple_formula(vars$outcome, c(vars$group, vars$visit, vars$covariates))
    expected <- as.formula(outcome ~ 1 + group + visit)

    expect_true(actual == expected)
})


test_that("sample_mvnorm", {

    # Sample singe value
    set.seed(3516)
    m <- 10
    z <- 16
    x <- replicate(n = 100000, {
        sample_mvnorm(m, z)
    })

    xm <- mean(x)
    xv <- var(x)

    expect_true(all( abs(xm - m) < (m - (m * 0.99))))
    expect_true(all( abs(xv - z) < (z - (z * 0.95))))


    # Sample multiple values
    set.seed(351)
    z <- as_vcov(c(5, 3, 4, 2), c(0.2, 0.4, 0.6, 0.3, 0.1, 0.7))
    m <- c(5, 15, 30, 45)

    x <- sample_mvnorm(m, z)
    expect_true(nrow(x) == 1)
    expect_true(ncol(x) == 4)

    vals <- replicate(n = 100000, {sample_mvnorm(m, z)})
    x2 <- matrix(unlist(vals), ncol = ncol(z), byrow = TRUE)
    x2_v <- var(x2)
    x2_m <- apply(x2, 2, mean)

    z_vec <- as.vector(z)

    expect_true(all(
        abs(x2_m - m) < (x2_m - (m * 0.98))
    ))

    expect_true(all(
        abs(as.vector(x2_v - z)) < (z_vec - (z_vec * 0.95))
    ))

})



test_that("record", {

    fun <- function(x) {
        return(x)
    }
    result_actual <- record(fun(iris))
    result_expected <- list(results = iris, warnings = NULL, errors = NULL, messages = NULL)
    expect_equal(result_actual, result_expected)


    fun <- function(x) {
        warning("w1")
        warning("w2")
        return(x)
    }
    result_actual <- record(fun(2))
    result_expected <- list(results = 2, warnings = c("w1", "w2"), errors = NULL, messages = NULL)
    expect_equal(result_actual, result_expected)


    fun <- function(x) {
        warning("w1")
        message("hi1")
        warning("w2")
        stop("an error")
        message("hi2")
        return(x)
    }
    expect_equal(
        record(fun(3)),
        list(results = list(), warnings = c("w1", "w2"), errors = c("an error"), messages = "hi1\n")
    )
})



test_that("is_absent",{


    expect_true(is_absent(NULL))
    expect_true(is_absent(NA))
    expect_true(is_absent(""))

    expect_true(is_absent(NULL, blank=FALSE))
    expect_true(is_absent(NA, blank = FALSE))
    expect_true(is_absent("", na = FALSE))

    expect_false(is_absent(NA, na = FALSE))
    expect_false(is_absent("", blank = FALSE))

    expect_false(is_absent("abc"))
    expect_false(is_absent(c("abc", "zya", NULL)))
    expect_false(is_absent(c("abc", NA, "adw")))
    expect_false(is_absent(c("adw", "")))
    expect_false(is_absent(1))
    expect_false(is_absent(c(1, 2, 3, NA)))
    expect_false(is_absent(factor(c("A", ""))))

})


test_that("str_contains",{

    expect_equal(
        str_contains(c("abcde", "xyzj", "faiwx"), c("x")),
        c(FALSE, TRUE, TRUE)
    )

    # Make sure its resistant to regex
    expect_equal(
        str_contains(c("abcde", "xyzj$", "^faiwx"), c("x")),
        c(FALSE, TRUE, TRUE)
    )

    expect_equal(
        str_contains(c("abcde", "xyzj$", "^faiwx"), c("x", "y", "z", "x", "q")),
        c(FALSE, TRUE, TRUE)
    )

    expect_equal(
        str_contains(c("abcde", "xyzj$", "^faiwx"), c("xyzj", "awdawd")),
        c(FALSE, TRUE, FALSE)
    )

})


test_that("sort_by", {
    x <- tibble(
        x = c(1, 1, 2, 2, 3, 3),
        y = c(1, 2, 1, 2, 1, 2),
        z = c("A", "B", "C", "D", "E", "F")
    )

    expect_equal(x, sort_by(x))
    expect_equal(x, sort_by(x, "z"))

    x2 <- x %>% sample_frac(1)

    expect_equal(x, sort_by(x2, "z"))
    expect_equal(x, sort_by(x2, c("x", "y")))

    expect_equal(arrange(x, desc(z)), sort_by(x, "z", T))
    expect_equal(arrange(x, x, desc(y)), sort_by(x, c("x", "y"), c(F, T)))
})




test_that("Stack", {
    mstack <- Stack$new()
    mstack$add(list(1, 2, 3, 4, 5, 6, 7))
    expect_equal(mstack$pop(3), list(1, 2, 3))
    expect_equal(mstack$pop(3), list(4, 5, 6))
    expect_equal(mstack$pop(3), list(7))
    expect_error(mstack$pop(1), "items to return")

    mstack <- Stack$new()
    mstack$add(list(1, 2, 3, 4))
    expect_equal(mstack$pop(3), list(1, 2, 3))
    mstack$add(list(5, 6))
    expect_equal(mstack$pop(3), list(4, 5, 6))
    mstack$add(list(7))
    expect_equal(mstack$pop(3), list(7))
    expect_error(mstack$pop(1), "items to return")
})


test_that("add_meta", {
    expect_equal(add_meta('a', 1), list(a=1))
    expect_equal(add_meta(c('a','b','c'), '1','2',3), list(a='1', b='2',c=3))
    expect_equal(add_meta(list('a'), 1), list(a=1))
    expect_error(add_meta(c('a','b','c'), 1, 'c'))
    expect_error(add_meta(c('a','b','c')))
    expect_error(add_meta(NULL, 'a'))
    expect_error(add_meta(NA, 'a'))
    expect_error(add_meta(c('a', NA), 1, 2))
})


test_that("assert_type", {
    expect_true(assert_type('a', is.character))
    expect_true(assert_type(c('a', 'b', 'c'), is.character))
    expect_true(assert_type(1, is.numeric))
    expect_true(assert_type(c(1,2,3), is.numeric))
    expect_true(assert_type(NA, is.na))
    expect_true(assert_type(NULL, is.null))
    expect_true(assert_type(list(), is.list))
    expect_true(assert_type(data.frame(), is.data.frame))
    expect_true(assert_type(environment(), is.environment))
    expect_true(assert_type(function() NULL, is.function))
    expect_true(assert_type(factor(), is.factor))
    expect_error(assert_type(factor('a'), is.character))
    expect_error(assert_type('a', is.numeric))
    expect_error(assert_type(1, is.null))
    v1 <- NULL
    expect_error(assert_type(v1, is.character))
    v2 <- NA
    expect_error(assert_type(v2, is.numeric))
    expect_error(assert_type(1, length))
})


test_that("assert_value", {
    expect_true(assert_value(max)(c(1,2,3), 3))
    expect_true(assert_value(length)(list(1,2), 2))
    expect_true(assert_value(names)(list(a=1,b=2,c=3), c('a', 'b', 'c')))
    expect_true(assert_value(mean)(c(a=1,b=2,c=3), 2))
    expect_true(assert_value(abs)(c(a=-1,b=2,c=-3), c(1,2,3)))
    f <- function(x) x * 5
    expect_true(assert_value(f)(c(a=1,b=2,c=3), c(a=5,b=10,c=15)))
    expect_error(assert_value(identity)(list(a=1,b=2,c=3), list(a=1,b=2,c=3)))
    expect_error(assert_value(min)(c(a=1,b=2,c=3), 7))
    expect_error(assert_value(median)(c(a=1,b=2,c=3), 1))
})

test_that("assert_anares_length", {
    expect_true(assert_anares_length('a', 1))
    expect_true(assert_anares_length(c(1,2), 2))
    expect_true(assert_anares_length(list(a=1,b=2), 2))
    expect_true(assert_anares_length(data.frame(a=1, b=2), 2))
    expect_true(assert_anares_length(list(), 0))
    expect_true(assert_anares_length(NULL, 0))
    expect_error(assert_anares_length(c('a', 'b', 'c'), 2))
    expect_error(assert_anares_length(c(1,2,3), 2))
    expect_error(assert_anares_length(list(), 2))
    expect_error(assert_anares_length(1, 2))
    expect_error(assert_anares_length(NA, 2))
})

test_that("make_chain", {
    is.numeric_or_na <- make_chain(any, is.numeric, is.na)
    expect_true(is.numeric_or_na(NA))
    expect_true(is.numeric_or_na(1))
    expect_false(is.numeric_or_na('a'))
    expect_false(is.numeric_or_na(list()))
    expect_error(make_chain(any, is.numeric, 'a')(1))
})

test_that("order_list_by_name", {
    expect_equal(order_list_by_name(list(a=1,b='x',c=TRUE), c("c", "a", "d", "x", "b", "t"))[[3]], 'x')
    expect_true(names(order_list_by_name(list(t=1,v='x'), c("c", "a", "d", "x", "b", "t"))) == 't')
    expect_true(all(names(order_list_by_name(list(t=1,v='x',z=2,m=list(), q='t', w=data.frame()), c("z", "t","q", "m"))) ==  c("z", "t","q", "m")))
    expect_length(order_list_by_name(list(u=1,v='x'), c("c", "a", "d", "x", "b", "t")), 0)
    expect_length(order_list_by_name(list(u=1,v='x'), c("c")), 0)
})

test_that("base_bind_rows", {
    l1 <- list(list(a=1,b=2), list(b=3, c=4))
    expect_equal(nrow(base_bind_rows(l1)), 2)
    expect_equal(ncol(base_bind_rows(l1)), 3)
    expect_equal(base_bind_rows(l1)[1, 2], 2)
    l2 <- list(list(a=1,b=2, c=3), list(a=1, b=3, c=4), list(a=9, b=10, c=12))
    expect_equal(nrow(base_bind_rows(l2)), 3)
    expect_equal(base_bind_rows(l2)[2,3], 4)
    expect_true(is.na(base_bind_rows(l1)[3,1]))
    expect_error(base_bind_rows(list(list(a=1,b=2), list(b=3, c=data.frame()))))
    l3 <- list(c(a=1,b=2), c(a=3, c=4))
    expect_error(base_bind_rows(l3))
})

test_that("namechecker", {
    chker <- namechecker('a', 'b', 'c', optional = c('d', 'e', 'f'))
    expect_type(chker, "closure")
    expect_equal(chker('musthave'), c('a', 'b', 'c'))
    expect_equal(chker('optional'), c('d', 'e', 'f'))
    expect_equal(chker('all'), c('a', 'b', 'c', 'd', 'e', 'f'))
    expect_type(chker('musthave_in_objnames'), "closure")
    expect_type(chker('objnames_in_musthave'), "closure")
    expect_true(all(chker('musthave_in_objnames')(list(a=1)) == list(a=TRUE, b=FALSE, c=FALSE)))
    expect_true(all(chker('musthave_in_objnames')(list(a=1, b = 2, c = 3, d = 4)) == list(a=TRUE, b=TRUE, c=TRUE)))
    expect_true(all(chker('objnames_in_musthave')(list(b=2, a = 1, d = 2, e = 4, x = 5)) == list(b=TRUE, a=TRUE, d=TRUE, e = TRUE, x = FALSE)))
    expect_true(all(chker('objnames_in_musthave')(list(f = 1, x = list(a=2))) == list(f=TRUE, x=FALSE)))
    expect_true(all(chker('objnames_in_musthave')(list(y = 1, x = list(a=2))) == list(y=FALSE, x=FALSE)))
})
