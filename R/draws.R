#' TODO
#'
#' @name draws
#' @param data TODO
#' @param data_ice TODO
#' @param vars TODO
#' @param method TODO
#'
#' @export
draws <- function(data, data_ice, vars, method) {
    UseMethod("draws", method)
}




#' @rdname draws
#' @export
draws.approxbayes <- function(data, data_ice, vars, method) {
    longdata <- longDataConstructor$new(data, vars)
    longdata$set_strategies(data_ice)
    x <- get_bootstrap_draws(longdata, method, use_samp_ids = FALSE, first_sample_orig = FALSE)
    return(x)
}


#' @rdname draws
#' @export
draws.condmean <- function(data, data_ice, vars, method) {
    longdata <- longDataConstructor$new(data, vars)
    longdata$set_strategies(data_ice)
    if (method$type == "bootstrap") {
        x <- get_bootstrap_draws(longdata, method, use_samp_ids = TRUE, first_sample_orig = TRUE)
    } else if (method$type == "jackknife") {
        x <- get_jackknife_draws(longdata, method)
    } else {
        stop("Unknown method type")
    }
    return(x)
}


#' TODO
#'
#' @description
#' TODO
#'
#' @param longdata TODO
#' @param method TODO
#' @param use_samp_ids TODO
#' @param first_sample_orig TODO
get_bootstrap_draws <- function(
    longdata,
    method,
    use_samp_ids = FALSE,
    first_sample_orig = FALSE
) {
    n_samples <- ife(first_sample_orig, method$n_samples + 1, method$n_samples)

    samples <- vector("list", length = n_samples)
    current_sample <- 1
    failed_samples <- 0
    failure_limit <- ceiling(method$threshold * n_samples)

    initial_sample <- get_mmrm_sample(
        ids = longdata$ids,
        longdata = longdata,
        method = method,
        optimizer = c("L-BFGS-B", "BFGS")
    )

    if (initial_sample$failed) {
        stop("Fitting MMRM to original dataset failed")
    }

    optimizer <- list(
        "L-BFGS-B" = NULL,
        "BFGS" = initial_sample[c("beta", "theta")]
    )

    if (first_sample_orig) {
        samples[[1]] <- initial_sample
        current_sample <- current_sample + 1
    }

    while (current_sample <= n_samples & failed_samples <= failure_limit) {

        ids_boot <- longdata$sample_ids()
        sample_boot <- get_mmrm_sample(
            ids = ids_boot,
            longdata = longdata,
            method = method,
            optimizer = optimizer
        )

        if (sample_boot$failed) {
            failed_samples <- failed_samples + 1
            if (failed_samples > failure_limit) {
                msg <- "More than %s failed fits. Try using a simpler covariance structure"
                stop(sprintf(msg, failure_limit))
            }
        } else {
            if (!use_samp_ids) {
                sample_boot$ids <- longdata$ids
            }
            samples[[current_sample]] <- sample_boot
            current_sample <- current_sample + 1
        }
    }
    ret <- as_draws(
        method = method,
        samples = as_sample_list(samples),
        data = longdata,
        formula = as_simple_formula(longdata$vars),
        n_failures = failed_samples
    )
    return(ret)
}


#' Title
#'
#' @param longdata TODO
#' @param method TODO
get_jackknife_draws <- function(longdata, method) {

    ids <- longdata$ids
    samples <- vector("list", length = length(ids) + 1)

    samples[[1]] <- get_mmrm_sample(
        ids = ids,
        longdata = longdata,
        method = method,
        optimizer = c("L-BFGS-B", "BFGS")
    )

    optimizer <- list(
        "L-BFGS-B" = NULL,
        "BFGS" = samples[[1]][c("beta", "theta")]
    )

    ids_jack <- lapply(seq_along(ids), function(i) ids[-i])

    for (i in seq_along(ids)) {
        ids_jack <- ids[-i]
        sample <- get_mmrm_sample(
            ids = ids_jack,
            longdata = longdata,
            method = method,
            optimizer = optimizer
        )
        if (sample$failed) {
            stop("Jackknife sample failed")
        }
        samples[[i + 1]] <- sample
    }
    ret <- as_draws(
        method = method,
        samples = as_sample_list(samples),
        data = longdata,
        formula = as_simple_formula(longdata$vars),
        n_failures = 0
    )
    return(ret)
}


#' TODO
#'
#' @description
#' TODO
#'
#' @param ids TODO
#' @param longdata TODO
#' @param method TODO
#' @param optimizer TODO
get_mmrm_sample <- function(ids, longdata, method, optimizer) {

    vars <- longdata$vars
    dat <- longdata$get_data(ids, nmar.rm = TRUE, na.rm = TRUE)
    model_df <- as_model_df(dat, as_simple_formula(vars))

    sample <- fit_mmrm_multiopt(
        designmat = model_df[, -1, drop = FALSE],
        outcome = as.data.frame(model_df)[, 1],
        subjid = dat[[vars$subjid]],
        visit = dat[[vars$visit]],
        group = dat[[vars$group]],
        cov_struct = method$covariance,
        REML = method$REML,
        same_cov = method$same_cov,
        optimizer = optimizer
    )

    if (sample$failed) {
        ret <- as_sample_single(
            ids = ids,
            failed = TRUE
        )
    } else {
        ret <- as_sample_single(
            ids = ids,
            failed = FALSE,
            beta = sample$beta,
            sigma = sample$sigma,
            theta = sample$theta
        )
    }
    return(ret)
}


#' Title - TODO
#'
#' @param longdata TODO
extract_data_nmar_as_na <- function(longdata) {
    # remove non-MAR data
    data <- longdata$get_data(longdata$ids, nmar.rm = FALSE, na.rm = FALSE)
    is_mar <- unlist(longdata$is_mar)
    data[!is_mar, longdata$vars$outcome] <- NA
    return(data)
}


#' @rdname draws
#' @export
draws.bayes <- function(data, data_ice, vars, method) {

    if (!is.na(method$seed)) {
        set.seed(method$seed)
    }

    longdata <- longDataConstructor$new(data, vars)
    longdata$set_strategies(data_ice)

    data2 <- extract_data_nmar_as_na(longdata)

    # compute design matrix
    frm <- as_simple_formula(vars)
    model_df <- as_model_df(data2, frm)

    # scale input data
    scaler <- scalerConstructor$new(model_df)
    model_df_scaled <- scaler$scale(model_df)

    # fit MMRM (needed for initial values)
    mmrm_initial <- fit_mmrm_multiopt(
        designmat = model_df_scaled[, -1, drop = FALSE],
        outcome = as.data.frame(model_df_scaled)[, 1],
        subjid = data2[[vars$subjid]],
        visit = data2[[vars$visit]],
        group = data2[[vars$group]],
        cov_struct = "us",
        REML = TRUE,
        same_cov = method$same_cov,
        optimizer = c("L-BFGS-B", "BFGS")
    )

    # run MCMC
    fit <- run_mcmc(
        designmat = model_df_scaled[, -1],
        outcome = model_df_scaled[, 1, drop = TRUE],
        group = data2[[vars$group]],
        sigma_reml = mmrm_initial$sigma,
        n_imputations = method$n_samples,
        burn_in = method$burn_in,
        seed = method$seed,
        burn_between = method$burn_between,
        initial_values = list(
            beta = mmrm_initial$beta,
            sigma = mmrm_initial$sigma
        ),
        same_cov = method$same_cov,
        verbose = method$verbose
    )

    # set names of covariance matrices
    fit$samples$sigma <- lapply(
        fit$samples$sigma,
        function(sample_cov) setNames(sample_cov, levels(data2[[vars$group]]))
    )

    # unscale samples
    samples <- mapply(
        function(x, y) list("beta" = x, "sigma" = y),
        lapply(fit$samples$beta, scaler$unscale_beta),
        lapply(fit$samples$sigma, function(covs) lapply(covs, scaler$unscale_sigma)),
        SIMPLIFY = FALSE
    )

    # set ids associated to each sample
    samples <- lapply(
        samples,
        function(x) as_sample_single(ids = longdata$ids, beta = x$beta, sigma = x$sigma, failed = FALSE)
    )

    result <- as_draws(
        method = method,
        samples = as_sample_list(samples),
        data = longdata,
        fit = fit$fit,
        formula = frm,
        n_failures = 0
    )

    return(result)
}




#' Print Draws Object
#'
#' @param x (`draws`)\cr input
#' @param ... not used
#' @export
print.draws <- function(x, ...) {

    frm <- as.character(x$formula)
    frm_str <- sprintf("%s ~ %s", frm[[2]], frm[[3]])

    meth <- switch(
         class(x$method)[[2]],
         "approxbayes" = "Approximate Bayes",
         "condmean" = "Conditional Mean",
         "bayes" = "Bayes"
    )

    meth_args <- vapply(
        mapply(
            function(x, y) sprintf("    %s: %s", y, x),
            x$method,
            names(x$method),
            USE.NAMES = FALSE,
            SIMPLIFY = FALSE
        ),
        identity,
        character(1)
    )

    n_samp <- length(x$samples)
    n_samp_string <- ife(
        has_class(x$method, "condmean"),
        sprintf("1 + %s", n_samp - 1),
        as.character(n_samp)
    )

    string <- c(
        "",
        "Draws Object",
        "------------",
        sprintf("Number of Samples: %s", n_samp_string),
        sprintf("Number of Failed Samples: %s", x$n_failures),
        sprintf("Model Formula: %s", frm_str),
        sprintf("Imputation Type: %s", class(x)[[2]]),
        "Method:",
        sprintf("    Type: %s", meth),
        meth_args,
        ""
    )

    cat(string, sep = "\n")
    return(invisible(x))
}




#' TODO
#'
#' @description
#' TODO
#'
#' @param ids TODO
#' @param beta TODO
#' @param sigma TODO
#' @param theta TODO
#' @param failed TODO
#' @param ids_samp TODO
as_sample_single <- function(
    ids,
    beta = NA,
    sigma = NA,
    theta = NA,
    failed = any(is.na(beta)),
    ids_samp = ids
) {
    x <- list(
        ids = ids,
        failed = failed,
        beta = beta,
        sigma = sigma,
        theta = theta,
        ids_samp = ids_samp
    )
    class(x) <- c("sample_single", "list")
    validate(x)
    return(x)
}


#' @export
validate.sample_single <- function(x, ...) {

    assert_that(
        x$failed %in% c(TRUE, FALSE),
        is.character(x$ids),
        length(x$ids) > 1,
        is.character(x$ids_samp),
        length(x$ids_samp) > 1
    )

    if (x$failed == TRUE) {
        assert_that(
            is.na(x$beta),
            is.na(x$sigma),
            is.na(x$theta)
        )
    } else {
        assert_that(
            is.numeric(x$beta),
            all(!is.na(x$beta)),
            is.list(x$sigma),
            !is.null(names(x$sigma)),
            all(vapply(x$sigma, is.matrix, logical(1)))
        )
    }
}


#' TODO
#'
#' @description
#' TODO
#'
#' @param ... TODO
as_sample_list <- function(...) {
    x <- list(...)
    if (length(x) == 1 & class(x[[1]])[[1]] != "sample_single") {
        x <- x[[1]]
    }
    class(x) <- c("sample_list", "list")
    validate(x)
    return(x)
}


#' @export
validate.sample_list <- function(x, ...) {
    assert_that(
        is.null(names(x)),
        all(vapply(x, function(x) class(x)[[1]] == "sample_single", logical(1))),
        all(vapply(x, function(x) validate(x), logical(1)))
    )
}


#' TODO
#'
#' @description
#' TODO
#'
#' @param method TODO
#' @param samples TODO
#' @param data TODO
#' @param formula TODO
#' @param n_failures TODO
#' @param fit TODO
as_draws <- function(
    method,
    samples,
    data,
    formula,
    n_failures = NULL,
    fit = NULL
) {
    x <- list(
        data = data,
        method = method,
        samples = samples,
        fit = fit,
        n_failures = n_failures,
        formula = formula
    )

    next_class <- switch(class(x$method)[[2]],
        "approxbayes" = "random",
        "condmean" = "condmean",
        "bayes" = "random"
    )

    class(x) <- c("draws", next_class, "list")
    return(x)
}


#' @export
validate.draws <- function(x, ...) {
    assert_that(
        has_class(x$data, "longdata"),
        has_class(x$method, "method"),
        has_class(x$samples, "sample_list"),
        validate(x$samples),
        is.null(x$n_failures) | is.numeric(x$n_failures),
        is.null(x$fit) | has_class(x$fit, "stanfit"),
        has_class(x$formula, "formula")
    )
}
