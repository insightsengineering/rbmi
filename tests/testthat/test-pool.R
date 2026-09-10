test_that("Rubin's rules", {
    set.seed(101)
    ests <- rnorm(100)
    ses <- rnorm(100, mean = 10, sd = 1)
    v_com <- seq(1, 1000, by = 100)
    n <- seq(16, 1015, by = 100)
    k <- 15

    actual_res <- sapply(
        v_com,
        function(i) rubin_rules(ests, ses, i),
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

    # if ses are all NA, then results are only for point estimate
    ses <- rep(NA, 100)
    v_com <- Inf
    expect_equal(
        rubin_rules(ests, ses, v_com),
        list(
            est_point = mean(ests),
            var_t = NA,
            df = NA
        )
    )
})


test_that("Rubin pooling uses t inference for continuous outcomes with infinite complete-data df", {
    results <- structure(
        list(
            est = c(0.2, 0.7, 1.1, 0.5),
            se = c(0.20, 0.25, 0.22, 0.24),
            df = rep(Inf, 4)
        ),
        class = "rubin"
    )
    rubin <- rubin_rules(results$est, results$se, Inf)
    expected_se <- sqrt(rubin$var_t)
    expected <- list(
        est = rubin$est_point,
        ci = rubin$est_point + c(-1, 1) * qt(0.975, rubin$df) * expected_se,
        se = expected_se,
        pvalue = 2 * pt(-abs(rubin$est_point / expected_se), df = rubin$df)
    )

    observed <- pool_internal(
        results,
        conf.level = 0.95,
        alternative = "two.sided",
        type = "percentile",
        D = NULL,
        outcome_type = "continuous"
    )

    expect_equal(observed, expected)
    expect_false(is.infinite(rubin$df))
})


test_that("Rubin pooling uses normal inference for non-continuous outcomes with infinite complete-data df", {
    results <- structure(
        list(
            est = c(0.2, 0.7, 1.1, 0.5),
            se = c(0.20, 0.25, 0.22, 0.24),
            df = rep(Inf, 4)
        ),
        class = "rubin"
    )
    rubin <- rubin_rules(results$est, results$se, Inf)
    expected_se <- sqrt(rubin$var_t)
    expected <- list(
        est = rubin$est_point,
        ci = rubin$est_point + c(-1, 1) * qnorm(0.975) * expected_se,
        se = expected_se,
        pvalue = 2 * pnorm(-abs(rubin$est_point / expected_se))
    )

    observed <- pool_internal(
        results,
        conf.level = 0.95,
        alternative = "two.sided",
        type = "percentile",
        D = NULL,
        outcome_type = "count"
    )

    expect_equal(observed, expected)
})


test_that("Rubin pooling retains t inference for finite complete-data df", {
    results <- structure(
        list(
            est = c(0.2, 0.7, 1.1, 0.5),
            se = c(0.20, 0.25, 0.22, 0.24),
            df = rep(80, 4)
        ),
        class = "rubin"
    )
    rubin <- rubin_rules(results$est, results$se, 80)
    expected_se <- sqrt(rubin$var_t)
    expected <- list(
        est = rubin$est_point,
        ci = rubin$est_point + c(-1, 1) * qt(0.975, rubin$df) * expected_se,
        se = expected_se,
        pvalue = 2 * pt(
            -abs(rubin$est_point / expected_se),
            df = rubin$df
        )
    )

    observed <- pool_internal(
        results,
        conf.level = 0.95,
        alternative = "two.sided",
        type = "percentile",
        D = NULL,
        outcome_type = "continuous"
    )

    expect_equal(observed, expected)
})


test_that("pool retains and reports transformed parameters separately", {
    analysis_results <- list(
        list(p1 = list(est = log(2), se = 0.20, df = Inf)),
        list(p1 = list(est = log(3), se = 0.25, df = Inf))
    )
    original_analysis <- as_analysis(
        results = analysis_results,
        method = method_bayes(n_samples = 2)
    )
    transformed_analysis <- as_analysis(
        results = analysis_results,
        method = method_bayes(n_samples = 2),
        transform = use_transform(exp(x))
    )

    original <- pool(original_analysis)
    transformed <- pool(transformed_analysis)
    expected <- original$pars$p1
    observed <- transformed$transformed_pars$p1

    expect_equal(transformed$pars, original$pars)
    expect_equal(observed$est, exp(expected$est))
    expect_equal(observed$ci, exp(expected$ci))
    expect_equal(observed$se, exp(expected$est) * expected$se)
    expect_equal(observed$pvalue, expected$pvalue)
    expect_equal(
        as.data.frame(transformed, scale = "original"),
        as.data.frame(original)
    )
    expect_equal(
        as.data.frame(transformed)$est,
        exp(as.data.frame(original)$est)
    )
    expect_output(print(transformed), "transformation applied")
    original_print <- capture.output(print(transformed, scale = "original"))
    expect_false(any(grepl("transformation applied", original_print, fixed = TRUE)))
    expect_error(
        as.data.frame(original, scale = "transformed"),
        "Transformed results are unavailable"
    )
    expect_error(
        print(original, scale = "transformed"),
        "Transformed results are unavailable"
    )
})


test_that("pool orders confidence limits after a decreasing transformation", {
    analysis <- as_analysis(
        results = list(
            list(p1 = list(est = 2, se = 0.20, df = Inf)),
            list(p1 = list(est = 3, se = 0.25, df = Inf))
        ),
        method = method_bayes(n_samples = 2),
        transform = use_transform(-x)
    )

    pooled <- pool(analysis)
    original_ci <- pooled$pars$p1$ci
    transformed_ci <- pooled$transformed_pars$p1$ci

    expect_equal(transformed_ci, rev(-original_ci))
    expect_lte(transformed_ci[1], transformed_ci[2])
})


test_that("pool rejects transformations that are not monotone over a confidence interval", {
    analysis <- as_analysis(
        results = list(
            list(p1 = list(est = -0.1, se = 1, df = Inf)),
            list(p1 = list(est = 0.1, se = 1, df = Inf))
        ),
        method = method_bayes(n_samples = 2),
        transform = use_transform(x^2)
    )

    expect_error(
        pool(analysis),
        "must be monotone over the pooled confidence interval"
    )
})


test_that("transformation supports estimate-only results and retains df", {
    analysis <- as_analysis(
        results = list(
            list(p1 = list(est = log(2))),
            list(p1 = list(est = log(3))),
            list(p1 = list(est = log(4)))
        ),
        method = method_condmean(n_samples = 2),
        transform = use_transform(exp(x))
    )
    observed <- pool(analysis, type = "normal")

    expect_true(is.finite(observed$transformed_pars$p1$est))
    expect_equal(
        observed$transformed_pars$p1$se,
        observed$transformed_pars$p1$est * observed$pars$p1$se
    )
    expect_equal(
        transform_pooled_par(
            list(
                est = log(2),
                ci = log(c(1.5, 2.5)),
                se = 0.1,
                pvalue = 0.05,
                df = 12
            ),
            use_transform(exp(x))
        )$df,
        12
    )
})


test_that("pval_percentile", {
    est <- c(0, rep(1, 3))
    pvals <- pval_percentile(est)
    expected <- c("pval_greater" = 0.2, "pval_less" = 0.8)
    expect_equal(pvals, expected)

    est <- rep(0, 4)
    pvals <- pval_percentile(est)
    expected <- c("pval_greater" = 1, "pval_less" = 1)
    expect_equal(pvals, expected)

    est <- c(0, rep(-1, 3))
    pvals <- pval_percentile(est)
    expected <- c("pval_greater" = 0.8, "pval_less" = 0.2)
    expect_equal(pvals, expected)

    est <- rep(1, 4)
    pvals <- pval_percentile(est)
    expected <- c("pval_greater" = 0, "pval_less" = 1)
    expect_equal(pvals, expected)

    est <- rep(-1, 4)
    pvals <- pval_percentile(est)
    expected <- c("pval_greater" = 1, "pval_less" = 0)
    expect_equal(pvals, expected)

    set.seed(101)
    est <- rnorm(10000)
    pvals <- pval_percentile(est)
    expect_true(all(pvals > 0.485 & pvals < 0.515)) # the "true" p-values are 0.5
})

test_that("get_ests_bmlmi", {
    ests <- rnorm(100)
    D <- 5

    res <- get_ests_bmlmi(ests, D)

    expect_true(is.list(res))
    expect_length(res, 3)
    expect_true(all(!is.na(res)))
    expect_true(all(!is.null(res)))
    expect_true(all(sapply(res, is.numeric)))
    expect_true(all(sapply(res, function(x) length(x) == 1)))
    expect_equal(round(res$est_point, 4), round(mean(ests), 4))

    D <- 3
    expect_error(
        get_ests_bmlmi(ests, D),
        regexp = "length of `ests` must be a multiple of `D`"
    )

    D <- 1
    expect_error(
        get_ests_bmlmi(ests, D),
        regexp = "`D` must be a numeric larger than 1"
    )

    ## Re-implement bmlmi Var & df implementations to ensure no
    ## small silly coding errors
    local_bmlmi <- function(x, D) {
        B <- length(x) / D
        M <- matrix(x, ncol = D, byrow = TRUE)
        point <- mean(M)
        est_B <- rowMeans(M)
        SSW <- sum(sweep(M, 1, est_B)^2)
        SSB <- D * sum((est_B - mean(est_B))^2)
        MSW <- SSW / ((B * D) - B)
        MSB <- SSB / (B - 1)
        V <- (1 / D) * (MSB * (1 + 1 / B) - MSW)
        df_num <- ((MSB * (B + 1)) - (MSW * B))^2
        df_den_1 <- ((MSB^2) * ((B + 1)^2)) / (B - 1)
        df_den_2 <- (MSW^2 * B) / (D - 1)
        df <- df_num / (df_den_1 + df_den_2)
        list(
            est_point = point,
            est_var = V,
            df = max(3, df)
        )
    }

    set.seed(3713)
    x <- c(
        rnorm(100, 10, sd = 2),
        rnorm(100, 5, sd = 7),
        rnorm(100, 20, sd = 3),
        rnorm(100, 30, sd = 4)
    )
    D <- 4
    expect_equal(
        local_bmlmi(x, D),
        get_ests_bmlmi(x, D)
    )

    set.seed(13)
    D <- 4
    x <- rnorm(100, 10, sd = 2)
    expect_equal(
        local_bmlmi(x, D),
        get_ests_bmlmi(x, D)
    )
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

        real_ci <- real_mu +
            c(-1, 1) * qnorm((1 - (1 - conf) / 2) * 1.005) * real_se
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
    expect_false(identical(x1, x2))
})

test_that("Pool (Rubin) works as expected when se = NA in analysis model", {
    set.seed(101)

    mu <- 0
    sd <- 1
    n <- 2000
    vals <- rnorm(n, mu, sd)
    real_mu <- mean(vals)

    runanalysis <- function(x) {
        list("p1" = list(est = mean(x), se = NA, df = NA))
    }

    results_bayes <- as_analysis(
        method = method_bayes(n_samples = 5000),
        results = lapply(
            seq_len(5000),
            function(x) runanalysis(sample(vals, size = n, replace = TRUE))
        )
    )
    bayes <- pool(results_bayes)
    bayes2 <- pool(results_bayes, conf.level = 0.8)
    bayes3 <- pool(results_bayes, alternative = "greater")

    expect_equal(
        bayes$pars$p1,
        list(
            est = real_mu,
            ci = as.numeric(c(NA, NA)),
            se = as.numeric(NA),
            pvalue = as.numeric(NA)
        ),
        tolerance = 1e-2
    )

    expect_equal(
        bayes2$pars$p1,
        list(
            est = real_mu,
            ci = as.numeric(c(NA, NA)),
            se = as.numeric(NA),
            pvalue = as.numeric(NA)
        ),
        tolerance = 1e-2
    )

    expect_equal(
        bayes3$pars$p1,
        list(
            est = real_mu,
            ci = as.numeric(c(NA, NA)),
            se = as.numeric(NA),
            pvalue = as.numeric(NA)
        ),
        tolerance = 1e-2
    )

    runanalysis <- function(x) {
        list("p1" = list(est = mean(x), se = NA, df = Inf))
    }

    results_bayes <- as_analysis(
        method = method_bayes(n_samples = 5000),
        results = lapply(
            seq_len(5000),
            function(x) runanalysis(sample(vals, size = n, replace = TRUE))
        )
    )
    bayes <- pool(results_bayes)
    bayes2 <- pool(results_bayes, conf.level = 0.8)
    bayes3 <- pool(results_bayes, alternative = "greater")

    expect_equal(
        bayes$pars$p1,
        list(
            est = real_mu,
            ci = as.numeric(c(NA, NA)),
            se = as.numeric(NA),
            pvalue = as.numeric(NA)
        ),
        tolerance = 1e-2
    )

    expect_equal(
        bayes2$pars$p1,
        list(
            est = real_mu,
            ci = as.numeric(c(NA, NA)),
            se = as.numeric(NA),
            pvalue = as.numeric(NA)
        ),
        tolerance = 1e-2
    )

    expect_equal(
        bayes3$pars$p1,
        list(
            est = real_mu,
            ci = as.numeric(c(NA, NA)),
            se = as.numeric(NA),
            pvalue = as.numeric(NA)
        ),
        tolerance = 1e-2
    )
})

test_that("pool BMLMI estimates", {
    set.seed(100)

    mu <- 0
    sd <- 1
    n <- 500
    B <- 1000
    D <- 10

    data <- rnorm(n, mu, sd)

    ############ NO MISSING VALUES

    boot_data <- lapply(
        seq.int(B),
        function(x) sample(data, size = n, replace = TRUE)
    )
    vals <- lapply(
        boot_data,
        function(x) {
            lapply(seq.int(D), function(y) {
                mu_est <- mean(x, na.rm = TRUE)
                sd_est <- sd(x, na.rm = TRUE)
                x[is.na(x)] <- rnorm(sum(is.na(x)), mu_est, sd_est) # impute
                return(x)
            })
        }
    ) %>%
        unlist(recursive = FALSE)

    runanalysis <- function(x) {
        list("p1" = list(est = mean(x), se = sqrt(var(x) / length(x)), df = NA))
    }

    ########  BMLMI
    results_bmlmi <- as_analysis(
        method = method_bmlmi(B = B, D = D),
        results = lapply(
            vals,
            runanalysis
        )
    )

    real_mu <- mean(sapply(vals, mean))

    means_per_boot <- lapply(
        split(vals, rep(seq.int(B), each = D)),
        function(x) sapply(x, function(y) mean(y))
    )
    # within bootstrap variability (0 if no missing values)
    var_within <- mean(sapply(means_per_boot, function(x) var(x)))
    real_se <- sqrt(
        mean(sapply(vals[seq(1, B * D, by = D)], function(x) var(x) / n)) +
            var_within
    )

    # real_se is the sum of the within and between bootstrap variability.
    # It should be similar to the se estimate from the bmlmi pooling method
    # (exact equality is not proven)

    pooled_res <- pool(results_bmlmi)

    expect_results <- function(res, real_mu, real_se) {
        conf <- res$conf.level

        pars <- res$pars[[1]]

        real_ci <- real_mu +
            c(-1, 1) * qnorm((1 - (1 - conf) / 2) * 1.005) * real_se
        ci <- pars$ci

        expect_true(real_ci[1] < ci[1] & real_ci[2] > ci[2])

        expect_true((real_mu - abs(real_mu * 0.01)) < pars$est)
        expect_true((real_mu + abs(real_mu * 0.01)) > pars$est)
    }

    expect_results(pooled_res, real_mu = real_mu, real_se = real_se)

    ############### WITH MISSING VALUES

    data[1:ceiling(n / 5)] <- NA # 20% missing values
    boot_data <- lapply(
        seq.int(B),
        function(x) sample(data, size = n, replace = TRUE)
    )
    vals_list <- lapply(
        boot_data,
        function(x) {
            lapply(seq.int(D), function(y) {
                mu_est <- mean(x, na.rm = TRUE)
                sd_est <- sd(x, na.rm = TRUE)
                x[is.na(x)] <- rnorm(sum(is.na(x)), mu_est, sd_est) # impute
                return(x)
            })
        }
    )
    vals <- unlist(vals_list, recursive = FALSE)

    ########  BMLMI
    results_bmlmi <- as_analysis(
        method = method_bmlmi(B = B, D = D),
        results = lapply(
            vals,
            runanalysis
        )
    )

    real_mu <- mean(sapply(vals, mean))

    means_per_boot <- lapply(
        split(vals, rep(seq.int(B), each = D)),
        function(x) sapply(x, function(y) mean(y))
    )
    var_within <- mean(sapply(means_per_boot, function(x) var(x))) # within bootstrap variability
    real_se <- sqrt(
        mean(sapply(vals[seq(1, B * D, by = D)], function(x) var(x) / n)) +
            var_within
    )

    # real_se should be similar to the se estimate from the bmlmi pooling method
    # (exact equality is not proven)

    pooled_res <- pool(results_bmlmi)

    expect_results(pooled_res, real_mu = real_mu, real_se = real_se)
    expect_true(sd / sqrt(n) < pooled_res$pars$p1$se)
})

test_that("Can recover known jackknife with  H0 < 0 & H0 > 0", {
    jest <- c(7, 3, 4, 5, 3, 3, 9)

    jest_r <- jest[-1]
    jest_m <- mean(jest_r)

    n <- length(jest_r)
    jest_se <- sqrt((sum((jest_r - jest_m)^2) * ((n - 1) / n)))

    expected <- list(
        est = 7,
        ci = 7 + c(-1, Inf) * qnorm(0.9) * jest_se,
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
        ci = 7 + c(-Inf, 1) * qnorm(0.9) * jest_se,
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
        ci = 7 + c(-1, 1) * qnorm(0.95) * jest_se,
        se = jest_se,
        pvalue = pnorm(7, sd = jest_se, lower.tail = FALSE) * 2
    )
    expect_equal(observed, expected)
})


test_that("Can recover known values using bootstrap percentiles", {
    best <- c(1, -1, -2, 3, 2, 1, -4, 3, 2)

    x <- quantile(best[-1], 0.9, type = 6)[[1]]
    pval <- pval_percentile(best[-1])[1]
    names(pval) <- NULL
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
        type = "percentile"
    )
    expect_equal(observed, expected)

    x <- quantile(best[-1], 0.1, type = 6)[[1]]
    pval <- pval_percentile(best[-1])[2]
    names(pval) <- NULL
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
        type = "percentile"
    )
    expect_equal(observed, expected)

    x1 <- quantile(best[-1], 0.10, type = 6)[[1]]
    x2 <- quantile(best[-1], 0.90, type = 6)[[1]]
    pval <- pval_percentile(best[-1])
    names(pval) <- NULL
    expected <- list(
        est = best[1],
        ci = c(x1, x2),
        se = NA,
        pvalue = min(pval) * 2
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type = "percentile"
    )
    expect_equal(observed, expected)
})


test_that("Results of bootstrap percentiles when n_samples = 0 or 1", {
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
        type = "percentile"
    )
    expect_equal(observed, expected)

    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "less",
        type = "percentile"
    )
    expect_equal(observed, expected)

    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type = "percentile"
    )
    expect_equal(observed, expected)

    best <- c(1, 3)
    expected <- list(
        est = best[1],
        ci = c(-Inf, 3),
        se = NA,
        pvalue = 0
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "greater",
        type = "percentile"
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
        type = "percentile"
    )
    expect_equal(observed, expected)

    expected <- list(
        est = best[1],
        ci = c(3, 3),
        se = NA,
        pvalue = 0
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type = "percentile"
    )
    expect_equal(observed, expected)
})

test_that("Bootstrap percentile does not return two-sided p-value larger than 1 when number of positive and negative estimates is equal", {
    best <- c(1, -1, -2, 3, 2, 1, -4, -3, 2)

    x1 <- quantile(best[-1], 0.10, type = 6)[[1]]
    x2 <- quantile(best[-1], 0.90, type = 6)[[1]]
    expected <- list(
        est = best[1],
        ci = c(x1, x2),
        se = NA,
        pvalue = 1
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type = "percentile"
    )
    expect_equal(observed, expected)
})


test_that("Can recover known values using bootstrap Normal", {
    best <- c(1, -1, -2, 3, 2, 1, -4, 3, 2)
    se <- sd(best[-1])

    expected <- list(
        est = best[1],
        ci = best[1] + c(-1, 1) * qnorm(0.96) * se,
        se = se,
        pvalue = pnorm(best[1], sd = se, lower.tail = FALSE) * 2
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.92,
        alternative = "two.sided",
        type = "normal"
    )
    expect_equal(observed, expected)

    expected <- list(
        est = best[1],
        ci = best[1] + c(-1, Inf) * qnorm(0.92) * se,
        se = se,
        pvalue = pnorm(best[1], sd = se, lower.tail = TRUE)
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.92,
        alternative = "less",
        type = "normal"
    )
    expect_equal(observed, expected)

    expected <- list(
        est = best[1],
        ci = best[1] + c(-Inf, 1) * qnorm(0.92) * se,
        se = se,
        pvalue = pnorm(best[1], sd = se, lower.tail = FALSE)
    )
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.92,
        alternative = "greater",
        type = "normal"
    )
    expect_equal(observed, expected)
})


test_that("Results of bootstrap Normal when n_samples = 0 or 1", {
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
        type = "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)

    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "less",
        type = "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)

    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type = "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)

    best <- c(1, 3)
    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "greater",
        type = "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)

    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.90,
        alternative = "less",
        type = "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)

    observed <- pool_internal.bootstrap(
        list(est = best),
        conf.level = 0.80,
        alternative = "two.sided",
        type = "normal"
    )
    observed <- lapply(observed, as.numeric)
    expect_equal(observed, expected)
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

test_that("mcse works as expected", {
    dat <- simulate_test_data(100) %>%
        as_tibble() %>%
        mutate(outcome = if_else(rbinom(n(), 1, 0.2) == 1, NA_real_, outcome))

    dat <- expand_locf(
        dat,
        id = levels(dat$id),
        visit = levels(dat$visit),
        vars = c("age", "group", "sex"),
        group = c("id"),
        order = c("id", "visit")
    )

    dat_ice <- dat %>%
        arrange(id, visit) %>%
        filter(is.na(outcome)) %>%
        group_by(id) %>%
        slice(1) %>%
        ungroup() %>%
        select(id, visit) %>%
        mutate(strategy = "JR")

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        covariates = c("sex*age", "visit*group")
    )

    method <- method_approxbayes(
        n_samples = 10
    )

    set.seed(987)
    drawObj <- draws(
        data = dplyr::group_by(dat, id),
        data_ice = dplyr::group_by(dat_ice, id),
        vars = vars,
        method = method,
        quiet = TRUE
    )

    imputeObj <- impute(drawObj, references = c("A" = "A", "B" = "A"))

    vars2 <- vars
    vars2$covariates <- c("sex*age")
    anaObj <- analyse(imputeObj, vars = vars2)
    poolObj <- pool(anaObj)

    result <- mcse(poolObj, anaObj)

    expect_true(is.list(result))

    result_df <- expect_silent(as.data.frame(result))

    expect_true(is.data.frame(result_df))
    expect_true(nrow(result_df) == length(poolObj$pars))

    # Snapshot tests can be flakey from minor changes in dependencies
    skip_if_not(is_core_test())
    expect_snapshot(print(result), cran = TRUE)
})

test_that("mcse honours the pooled reporting transformation", {
    analysis <- as_analysis(
        results = list(
            list(p1 = list(est = log(2), se = 0.20, df = Inf)),
            list(p1 = list(est = log(3), se = 0.25, df = Inf)),
            list(p1 = list(est = log(4), se = 0.30, df = Inf))
        ),
        method = method_bayes(n_samples = 3),
        transform = use_transform(exp(x))
    )
    pooled <- pool(analysis)
    observed <- mcse(pooled, analysis)
    original_jackknife <- lapply(
        seq_len(pooled$N),
        mcse_jackknife,
        results = analysis,
        conf.level = pooled$conf.level,
        alternative = pooled$alternative
    )
    expected <- mcse_combine_all_pars(lapply(
        original_jackknife,
        transform_pooled_pars,
        transform = analysis$transform
    ))

    expect_equal(observed$transformed_pars, expected)
    expect_equal(as.data.frame(observed), as_data_frame_internal(observed, expected))
    expect_equal(
        as.data.frame(observed, scale = "original"),
        as_data_frame_internal(observed)
    )
    expect_output(print(observed), "transformation applied")
})
