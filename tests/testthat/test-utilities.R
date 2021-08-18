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
    z <- as_covmat(c(5, 3, 4, 2), c(0.2, 0.4, 0.6, 0.3, 0.1, 0.7))
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



test_that("record_warnings", {

    fun <- function(x) {
        return(x)
    }
    result_actual <- record_warnings(fun(iris))
    result_expected <- list(results = iris, warnings = NULL)
    expect_equal(result_actual, result_expected)


    fun <- function(x) {
        warning("w1")
        warning("w2")
        return(x)
    }
    result_actual <- record_warnings(fun(2))
    result_expected <- list(results = 2, warnings = c("w1", "w2"))
    expect_equal(result_actual, result_expected)


    fun <- function(x) {
        warning("w1")
        warning("w2")
        stop("an error")
        return(x)
    }
    expect_error(record_warnings(fun(2)), "an error")
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