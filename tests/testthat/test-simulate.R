

suppressPackageStartupMessages({
    library(dplyr)
})


test_that("simulate data", {
    # Show that the function respects the seed and that

    set.seed(120)
    x <- simulate_data()

    set.seed(120)
    y <- simulate_data(
        n = 200,
        sd = c(3, 5, 7),
        cor = c(0.1, 0.7, 0.4),
        mu = list(
            int = 10,
            age = 3,
            sex = 2,
            trt = c(0, 4, 8),
            visit =c(0, 1, 2)
        )
    )

    set.seed(121)
    z <- simulate_data()

    expect_true(identical(x,y))
    expect_false(identical(x,z))


    # Show that we can recover known values
    set.seed(3918)
    dat <- simulate_data(
        n = 4000,
        sd = c(1, 2,3),
        cor = c(0, 0, 0.),
        mu = list(
            int = 10,
            age = 3,
            sex = 2,
            trt = c(0, 8, 17),
            visit =c(0, 3, 6)
        )
    )

    mod <- lm(data = dat, outcome ~ age + sex+ group* visit)

    m <- coef(mod)
    v <- sqrt(diag(vcov(mod)))

    lci <- m - qnorm(0.99) * v
    uci <- m + qnorm(0.99) * v

    real <- c(10, 3, 2, 0, 3, 6, 8 , 17)

    expect_true(all(lci <= real & real <= uci))
})



test_that("vcov", {
    actual <- as_vcov(c(1,2), 0)
    expected <- matrix(c(1,0,0,4), byrow = TRUE, nrow= 2)
    expect_equal(actual, expected)


    actual <- as_vcov(c(2,4), 0.5)
    expected <- matrix(c(4,4,4,16), byrow = TRUE, nrow= 2)
    expect_equal(actual, expected)


    actual <- as_vcov(c(2,4, 8), c(0.4, 0.5, 0.6))
    expected <- matrix(
        c(4,3.2,8,
          3.2,16,19.2,
          8,19.2, 64),
        byrow = TRUE,
        nrow= 3
    )
    expect_equal(actual, expected)
})







