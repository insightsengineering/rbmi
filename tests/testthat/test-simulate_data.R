mu <- c(1,2,3)
sigma <- rbind(c(3,0.5,0.3),c(0.5,4,0.5),c(0.3,0.5,5))
n <- 10


test_that("set_simul_pars", {

    pars <- set_simul_pars(
        mu = mu,
        sigma = sigma,
        n = n)

    expect_equal(list(pars$mu, pars$sigma, pars$n) , list(mu, sigma, n))

    expect_true(
        all(c(pars$prob_ice1, pars$prob_post_ice1_dropout, pars$prob_dropout, pars$prob_miss) == 0)
    )
    expect_true(
        pars$or_outcome_ice1 == 1
    )

    expect_true(validate(pars))

    expect_equal(class(pars), "simul_pars")

    pars$prob_dropout <- NULL
    expect_error(validate(pars))

    pars$prob_dropout <- 2
    expect_error(validate(pars))

    pars$prob_dropout <- "0.5"
    expect_error(validate(pars))

    expect_error(set_simul_pars(mu = mu))

})


test_that("", {})
