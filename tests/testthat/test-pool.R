test_that("Rubin's rules", {

    set.seed(101)
    ests <- rnorm(100)
    ses <- rnorm(100, mean = 10, sd = 1)
    v_com <- seq(1, 1000, by = 100)
    n <- seq(16, 1015, by = 100)
    k <- 15

    actual_res <- sapply(
        v_com, function(i) rubin_rules(ests, ses, i),
        simplify = FALSE
    )

    expect_equal(actual_res, mice_res1)


    # check when no variability in estimates (i.e. when no missing values)
    ests_allequal <- rep(0, 100)

    actual_res <- sapply(
        v_com,
        function(i) rubin_rules(ests_allequal, ses, i),
        simplify = FALSE
    )
    expect_equal(actual_res, mice_res2, tolerance = 10e-4)


    # check when v_com <- Inf
    v_com <- Inf
    actual_res <- rubin_rules(ests, ses, v_com)
    expect_equal(actual_res, mice_res3)


    # when v_com = NA or v_com = Inf and there are no missing values, df = Inf
    v_com <- c(Inf, NA)
    actual_res <- sapply(
        v_com,
        function(i) rubin_rules(ests_allequal, ses, i)$df
    )

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
            lapply(
                seq_len(n_boot),
                function(x) runanalysis(sample(vals, size = n, replace = TRUE))
            )
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



    x1 <- pool(results_boot, type = "normal", alternative = "less")
    x2 <- pool(results_boot, type = "percentile", alternative = "less")
    expect_false(identical(x1,x2))


})





test_that("Can recover known jackknife with  H0 < 0 & H0 > 0", {
    jest <- c( 7, 3, 4 , 5 , 3 ,3 , 9)

    jest_r <- jest[-1]
    jest_m <- mean(jest_r)

    n <- length(jest_r)
    jest_se <- sqrt((sum((jest_r - jest_m)^2) * ((n-1) / n)) )

    expected <- list(
        est = 7,
        ci = 7 + c(-1,Inf) * qnorm(0.9) * jest_se,
        se = jest_se,
        pvalue = pnorm(7, sd = jest_se, lower.tail = TRUE)
    )
    observed <- pool_internal.jackknife(
        list(est = jest),
        conf.level = 0.90,
        alternative = "less"
    )
    expect_equal(observed, expected)



    observed <- pool_internal.jackknife(
        list(est = jest),
        conf.level = 0.90,
        alternative = "greater"
    )
    expected <- list(
        est = 7,
        ci = 7 + c(-Inf,1) * qnorm(0.9) * jest_se,
        se = jest_se,
        pvalue = pnorm(7, sd = jest_se, lower.tail = FALSE)
    )
    expect_equal(observed, expected)



    observed <- pool_internal.jackknife(
        list(est = jest),
        conf.level = 0.90,
        alternative = "two.sided"
    )
    expected <- list(
        est = 7,
        ci = 7 + c(-1,1) * qnorm(0.95) * jest_se,
        se = jest_se,
        pvalue = pnorm(7, sd = jest_se, lower.tail = FALSE) * 2
    )
    expect_equal(observed, expected)

})






test_that("Can recover known values using bootstrap percentiles", {
    best <- c(1,-1,-2,3,2,1,-4,3,2)

    x <- quantile(best[-1], 0.9, type = 6)[[1]]
    pval <- (sum(best[-1] < 0) + 1 ) / length(best)
    expected <- list(
        est = best[1],
        ci = c(-Inf, x),
        se = NA,
        pvalue = pval
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "greater",
        type =  "percentile"
    )
    expect_equal(observed, expected)



    x <- quantile(best[-1], 0.1, type = 6)[[1]]
    pval <- (sum(best[-1] > 0) + 1 ) / length(best)
    expected <- list(
        est = best[1],
        ci = c(x, Inf),
        se = NA,
        pvalue = pval
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "less",
        type =  "percentile"
    )
    expect_equal(observed, expected)



    x1 <- quantile(best[-1], 0.10, type = 6)[[1]]
    x2 <- quantile(best[-1], 0.90, type = 6)[[1]]
    pval <- (sum(best[-1] < 0) + 1 ) / length(best)
    expected <- list(
        est = best[1],
        ci = c(x1, x2),
        se = NA,
        pvalue = pval * 2
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type =  "percentile"
    )
    expect_equal(observed, expected)
})



test_that("Results of bootstrap percentiles when n_samples = 0 or 1", {

    ################### n_samples = 0

    best <- c(1)
    expected <- list(
        est = best[1],
        ci = c(NA, NA),
        se = NA,
        pvalue = NA
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "greater",
        type =  "percentile"
    )
    expect_equal(observed, expected)


    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "less",
        type =  "percentile"
    )
    expect_equal(observed, expected)


    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type =  "percentile"
    )
    expect_equal(observed, expected)




    ################## n_samples = 1

    best <- c(1,3)
    expected <- list(
        est = best[1],
        ci = c(-Inf, 3),
        se = NA,
        pvalue = 0.5
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "greater",
        type =  "percentile"
    )
    expect_equal(observed, expected)



    expected <- list(
        est = best[1],
        ci = c(3, Inf),
        se = NA,
        pvalue = 1
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "less",
        type =  "percentile"
    )
    expect_equal(observed, expected)



    expected <- list(
        est = best[1],
        ci = c(3, 3),
        se = NA,
        pvalue = 1
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type =  "percentile"
    )
    expect_equal(observed, expected)
}
)

test_that("Bootstrap percentile does not return two-sided p-value larger than 1 when number of positive and negative estimates is equal", {
    best <- c(1,-1,-2,3,2,1,-4,-3,2)

    x1 <- quantile(best[-1], 0.10, type = 6)[[1]]
    x2 <- quantile(best[-1], 0.90, type = 6)[[1]]
    pval <- (sum(best[-1] < 0) + 1 ) / length(best)
    expected <- list(
        est = best[1],
        ci = c(x1, x2),
        se = NA,
        pvalue = 1 # 2*pval is larger than one in this case: (n_samples+2)/(n_samples+1)
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type =  "percentile"
    )
    expect_equal(observed, expected)

})




test_that("Can recover known values using bootstrap Normal", {

    best <- c(1,-1,-2,3,2,1,-4,3,2)
    se <- sd(best[-1])

    expected <- list(
        est = best[1],
        ci = best[1] + c(-1,1) * qnorm(0.96) * se,
        se = se,
        pvalue = pnorm(best[1], sd=se, lower.tail = FALSE) * 2
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.92,
        alternative = "two.sided",
        type =  "normal"
    )
    expect_equal(observed, expected)



    expected <- list(
        est = best[1],
        ci = best[1] + c(-1,Inf) * qnorm(0.92) * se,
        se = se,
        pvalue = pnorm(best[1], sd=se, lower.tail = TRUE)
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.92,
        alternative = "less",
        type =  "normal"
    )
    expect_equal(observed, expected)



    expected <- list(
        est = best[1],
        ci = best[1] + c(-Inf,1) * qnorm(0.92) * se,
        se = se,
        pvalue = pnorm(best[1], sd=se, lower.tail = FALSE)
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.92,
        alternative = "greater",
        type =  "normal"
    )
    expect_equal(observed, expected)
})




test_that("Results of bootstrap Normal when n_samples = 0 or 1", {

    ################### n_samples = 0

    best <- c(1)
    expected <- list(
        est = best[1],
        ci = as.numeric(c(NA, NA)),
        se = as.numeric(NA),
        pvalue = as.numeric(NA)
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "greater",
        type =  "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)


    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "less",
        type =  "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)


    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type =  "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)




    ################## n_samples = 1

    best <- c(1,3)
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "greater",
        type =  "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)


    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "less",
        type =  "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)


    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type =  "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)
}
)




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
        results = replicate(
            n = 51,
            runanalysis(rnorm(n, mu, sd)),
            simplify = FALSE
        )
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
