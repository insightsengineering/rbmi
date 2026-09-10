suppressPackageStartupMessages({
    library(testthat)
})


test_that("count Stan model is reused from the model cache", {
    skip_if_not(is_core_test())

    local_cache_dir <- withr::local_tempdir()
    withr::local_options(
        rbmi.enable_cache = TRUE,
        rbmi.cache_dir = local_cache_dir
    )

    set.seed(4812)
    expected_random_value <- stats::runif(1)
    set.seed(4812)
    first_model <- expect_silent(get_stan_model_count())
    expect_equal(stats::runif(1), expected_random_value)
    expect_s4_class(first_model, "stanmodel")

    stan_files <- list.files(
        local_cache_dir,
        pattern = "^rbmi_count_model_.*[.]stan$",
        full.names = TRUE
    )
    expect_length(stan_files, 1)
    rds_file <- sub("[.]stan$", ".rds", stan_files)
    expect_true(file.exists(rds_file))

    # Let rstan unload the first model's DSO so it can safely restore the
    # serialized model on the next call.
    rm(first_model)
    invisible(gc())

    old_stan_time <- as.POSIXct("2000-01-01", tz = "UTC")
    future_rds_time <- Sys.time() + 3600
    Sys.setFileTime(stan_files, old_stan_time)
    Sys.setFileTime(rds_file, future_rds_time)

    second_model <- expect_silent(get_stan_model_count())
    expect_s4_class(second_model, "stanmodel")
    expect_equal(
        as.numeric(file.info(stan_files)$mtime),
        as.numeric(old_stan_time)
    )
    expect_equal(
        as.numeric(file.info(rds_file)$mtime),
        as.numeric(future_rds_time)
    )
})


test_that("count MCMC returns named group-specific dispersion draws", {
    skip_if_not(is_core_test())

    dat <- utils::read.csv(system.file(
        "extdata",
        "sim_rogeretal.csv",
        package = "rbmi"
    ))
    dat <- dat[, c(
        "patient",
        "period",
        "TreatLab",
        "OnOff",
        "Length",
        "BaseCount",
        "Observed_Count"
    )]
    dat$patient <- factor(dat$patient)
    dat$period <- as.character(dat$period)
    dat$TreatLab <- factor(
        dat$TreatLab,
        levels = c("Placebo", "100mg", "300mg")
    )
    dat <- expand_locf(
        dat,
        patient = levels(dat$patient),
        period = as.character(1:3),
        vars = c("TreatLab", "BaseCount"),
        group = "patient",
        order = c("patient", "period")
    )
    dat$OnOff <- factor(
        ifelse(dat$period == "1", "On", "Off"),
        levels = c("On", "Off")
    )
    dat$Length[is.na(dat$Length)] <- 0

    dat_ice <- unique(
        dat[dat$period == "3" & dat$Length > 0, "patient", drop = FALSE]
    )
    dat_ice$strategy <- "MAR"
    vars <- set_vars(
        outcome = "Observed_Count",
        subjid = "patient",
        period = "period",
        duration = "Length",
        group = "TreatLab",
        covariates = c("TreatLab:OnOff", "TreatLab:BaseCount")
    )
    method <- method_bayes(
        same_cov = FALSE,
        n_samples = 2,
        control = control_bayes(
            warmup = 30,
            thin = 1,
            chains = 1,
            seed = 1821,
            control = list(adapt_delta = 0.9)
        )
    )

    result <- suppressWarnings(draws(
        outcome = "count",
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method,
        quiet = TRUE
    ))

    expect_s3_class(result, "draws_count")
    expect_length(result$samples, 2)
    expect_true(all(vapply(
        result$samples,
        function(x) identical(names(x$phi), levels(dat$TreatLab)),
        logical(1)
    )))
})
