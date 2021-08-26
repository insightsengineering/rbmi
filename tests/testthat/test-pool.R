suppressPackageStartupMessages({
    library(mice)
})


test_that("Rubin's rules", {

              set.seed(101)
              ests <- rnorm(100)
              ses <- rnorm(100, mean = 10, sd = 1)
              v_com <- seq(1, 1000, by = 100)
              n <- seq(16, 1015, by = 100)
              k <- 15


              actual_res <- sapply(v_com, function(i) rubin_rules(ests, ses, i), simplify = FALSE)
              mice_res <- sapply(n, function(i) mice::pool.scalar(ests, ses^2, n = i, k = k), simplify = FALSE)

              mice_res <- lapply(
                  mice_res,
                  function(x) {
                      x <- x[names(x) %in% c("qbar", "t", "df")]
                      names(x) <- names(actual_res[[1]])
                      return(x)
                  }
              )

              expect_equal(actual_res, mice_res)



              # check when no variability in estimates (i.e. when no missing values)
              ests_allequal <- rep(0, 100)

              actual_res <- sapply(v_com, function(i) rubin_rules(ests_allequal, ses, i), simplify = FALSE)
              mice_res <- sapply(n, function(i) mice::pool.scalar(ests_allequal, ses^2, n = i, k = k), simplify = FALSE)

              mice_res <- lapply(
                  mice_res,
                  function(x) {
                      x <- x[names(x) %in% c("qbar", "t", "df")]
                      names(x) <- names(actual_res[[1]])
                      return(x)
                  }
              )

              expect_equal(actual_res, mice_res, tolerance = 10e-4)



              # check when v_com <- Inf
              v_com <- Inf

              actual_res <- rubin_rules(ests, ses, v_com)
              mice_res <- mice::pool.scalar(ests, ses^2, n = 1e20, k = 1)

              mice_res <- mice_res[names(mice_res) %in% c("qbar", "t", "df")]
              names(mice_res) <- names(actual_res)

              expect_equal(actual_res, mice_res)



              # when v_com = NA or v_com = Inf and there are no missing values, df = Inf
              v_com <- c(Inf, NA)
              actual_res <- sapply(v_com, function(i) rubin_rules(ests_allequal, ses, i)$df)

              expect_true(all(actual_res == Inf))


          })




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
