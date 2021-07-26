



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

    results_boot <- append(
        list(runanalysis(vals)),
        lapply(seq_len(n_boot), function(x) runanalysis(sample(vals, size = n, replace = TRUE)))
    ) %>%
        as_class("bootstrap")


    ########  Jackknife

    results_jack <- append(
        list(runanalysis(vals)),
        lapply(seq_len(n), function(i) runanalysis(vals[-i]))
    ) %>%
        as_class("jackknife")



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




    ### TODO - Need to implement actual tests

    pool(results_boot, type = "normal", alternative = "less")
    pool(results_boot, type = "percentile", alternative = "less")
    pool(results_jack, alternative = "less")

    pool(results_boot, type = "normal", alternative = "greater")
    pool(results_boot, type = "percentile", alternative = "greater")
    pool(results_jack, alternative = "greater")


    results <- results_boot %>% as_class("rubin")
    pool(results)


})
