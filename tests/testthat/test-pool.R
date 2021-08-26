



test_that("pool", {
    set.seed(101)

    mu <- 0
    sd <- 1
    n <- 2000
    n_boot <- 5000
    vals <- rnorm(n, mu, sd)

    runanalysis <- function(x) {
        list("p1" = list(est = mean(x), se = sqrt(var(x) / length(x)), df = NA))
    }


    ########  Bootstrap
    results_boot <- as_analysis(
        method = method_condmean(n_samples = 5000),
        results = append(
            list(results = runanalysis(vals)),
            lapply(seq_len(n_boot), function(x) runanalysis(sample(vals, size = n, replace = TRUE)))
        )
    )

    ########  Jackknife
    results_jack <- as_analysis(
        method = method_condmean(type = "jackknife"),
        results = append(
            list(runanalysis(vals)),
            lapply(seq_len(n), function(i) runanalysis(vals[-i]))
        )
    )

    ###### reference CIs
    real_mu <- mean(vals)
    real_se <- sqrt(var(vals) / n)


    boot_norm <- pool(results_boot, type = "normal")
    boot_perc <- pool(results_boot, type = "percentile")
    jack <- pool(results_jack)


    expect_results <- function(res, real_mu, real_se) {
        conf <- res$conf.level

        pars <- res$pars[[1]]

        real_ci <- real_mu + c(-1, 1) * qnorm( (1 - (1 - conf) / 2) * 1.005) * real_se
        ci <- pars$ci

        expect_true(real_ci[1] < ci[1] & real_ci[2] > ci[2])

        expect_true((real_mu - abs(real_mu * 0.01)) < pars$est)
        expect_true((real_mu + abs(real_mu * 0.01)) > pars$est)

    }

    expect_results(boot_norm, real_mu, real_se)
    expect_results(boot_perc, real_mu, real_se)
    expect_results(jack, real_mu, real_se)



    ### TODO - Need to implement actual tests + rubin

    pool(results_boot, type = "normal", alternative = "less")
    pool(results_boot, type = "percentile", alternative = "less")
    pool(results_jack, alternative = "less")

    pool(results_boot, type = "normal", alternative = "greater")
    pool(results_boot, type = "percentile", alternative = "greater")
    pool(results_jack, alternative = "greater")

})




test_that("condmean doesn't use first element in CI", {

    mu <- 5
    sd <- 10
    n <- 200

    runanalysis <- function(x) {
        list("p1" = list(est = mean(x)))
    }

    set.seed(2040)
    x <- as_analysis(
        method = method_condmean(n_samples = 50),
        results = replicate(n = 51, runanalysis(rnorm(n, mu, sd)), simplify = FALSE)
    )


    pooled_1 <- pool(x)
    expect_equal(pooled_1$pars$p1$est, x$results[[1]]$p1$est)

    x$results[[1]]$p1$est <- 9999

    pooled_2 <- pool(x)
    expect_equal(pooled_2$pars$p1$est, x$results[[1]]$p1$est)

    pooled_1$pars$p1$est <- NULL
    pooled_2$pars$p1$est <- NULL
    expect_equal(pooled_1, pooled_2)
})
