

#' TODO
#'
#' @name pool
#' @param results TODO
#' @param conf.level TODO
#' @param alternative TODO
#' @param type TODO
#'
#' @export
pool <- function(
    results,
    conf.level = 0.95,
    alternative = c("two.sided", "less", "greater"),
    type = c("percentile", "normal")
) {

    assert_that(
        class(results)[[1]] == "analysis"
    )
    validate(results)

    alternative <- match.arg(alternative)
    type <- match.arg(type)

    assert_that(
        is.numeric(conf.level),
        length(conf.level) == 1,
        0 < conf.level & conf.level < 1,
        msg = "`conf.level` must be between 0 and 1"
    )

    pool_type <- class(results$results)[[1]]

    results_transpose <- transpose_results(results$results, get_pool_components(pool_type))

    pars <- lapply(
        results_transpose,
        function(x, ...) pool_(as_class(x, pool_type), ...),
        conf.level = conf.level,
        alternative = alternative,
        type = type
    )

    if (pool_type == "bootstrap") {
        method <- sprintf("%s (%s)", pool_type, type)
    } else {
        method <- pool_type
    }

    ret <- list(
        pars = pars,
        conf.level = conf.level,
        alternative = alternative,
        N = length(results$results),
        method = method
    )
    class(ret) <- "pool"
    return(ret)
}


#' TODO
#'
#' @param x TODO
get_pool_components <- function(x) {
    switch(x,
           "rubin" = c("est", "df", "se"),
           "jackknife" = c("est"),
           "bootstrap" = c("est")
    )
}


#' @rdname pool
#' @export
pool_ <- function(results, conf.level, alternative, type) {
    UseMethod("pool_")
}


#' @importFrom stats qnorm pnorm
#' @rdname pool
#' @export
pool_.jackknife <- function(results, conf.level, alternative, type) {
    alpha <- 1 - conf.level
    ests <- results$est
    est_point <- ests[1]
    ests_jack <- ests[-1] # First estimate is full dataset
    mean_jack <- mean(ests_jack)
    N_jack <- length(ests_jack)
    se_jack <- sqrt(((N_jack - 1) / N_jack) * sum((ests_jack - mean_jack)^2))
    ret <- normal_ci(est_point, se_jack, alpha, alternative, qnorm, pnorm)
    return(ret)
}




#' @rdname pool
#' @export
pool_.bootstrap <- function(
    results,
    conf.level,
    alternative,
    type = c("percentile", "normal")
) {
    type <- match.arg(type)
    bootfun <- switch(
        type,
        percentile = pool_bootstrap_percentile,
        normal = pool_bootstrap_normal
    )

    ret <- bootfun(results$est, conf.level, alternative)
    return(ret)
}

#' @title TODO
#'
#' @description TODO
#'
#' @param v_com TODO
#' @param var_b TODO
#' @param var_t TODO
#' @param M TODO
#'
#' @return TODO
rubin_df <- function(v_com, var_b, var_t, M) {

    if (is.na(v_com) || (is.infinite(v_com) & var_b == 0)) {
        df <- Inf
    } else {
        lambda <- (1 + 1 / M) * var_b / var_t

        if(!is.infinite(v_com)) {
            v_obs <- ((v_com + 1) / (v_com + 3)) * v_com * (1 - lambda)
        }

        if(lambda != 0) {
            v_old <- (M - 1)  / lambda^2
            if(is.infinite(v_com))
                df <- v_old
            else {
                df <- (v_old * v_obs) / (v_old + v_obs)
            }
        } else {
            df <- v_obs
        }
    }

    return(df)
}

#' @title TODO
#'
#' @description TODO
#'
#' @param ests TODO
#' @param ses TODO
#' @param v_com TODO
#'
#' @return TODO
rubin_rules <- function(ests, ses, v_com) {

    M <- length(ests)
    est_point <- mean(ests)

    var_w <- mean(ses^2)
    var_b <- var(ests)
    var_t <- var_w + var_b + var_b / M

    df <- rubin_df(
        v_com = v_com,
        var_b = var_b,
        var_t = var_t,
        M = M
    )

    ret_obj <- list(
        est_point = est_point,
        var_t = var_t,
        df = df
    )

    return(ret_obj)
}

#' @importFrom stats qt pt var
#' @rdname pool
#' @export
pool_.rubin <- function(results, conf.level, alternative, type) {
    ests <- results$est
    ses <- results$se
    dfs <- results$df
    alpha <- 1 - conf.level

    v_com <- unique(dfs)

    assert_that(
        all(!is.na(ses)),
        msg = "Standard Errors for Rubin's rules can not be NA"
    )

    assert_that(
        length(v_com) == 1,
        msg = "Degrees of freedom should be consistent across all samples"
    )

    res_rubin <- rubin_rules(
        ests = ests,
        ses = ses,
        v_com = v_com
    )

    ret <- normal_ci(
        point = res_rubin$est_point,
        se = sqrt(res_rubin$var_t),
        alpha = alpha,
        alternative = alternative,
        qfun = qt,
        pfun = pt,
        df = res_rubin$df
    )

    return(ret)
}




#' Title
#' @description
#' TODO
#' @param est TODO
#' @param conf.level TODO
#' @param alternative TODO
pool_bootstrap_percentile <- function(est, conf.level, alternative) {
    est_orig <- est[1]
    est <- est[-1]
    alpha <- 1 - conf.level
    pvals <- (c(sum(est < 0), sum(est > 0)) + 1) / (length(est) + 1)

    index <- switch(alternative,
                    two.sided = c(1, 2),
                    greater = 1,
                    less = 2
    )

    quant_2_side <- quantile(est, probs = c(alpha / 2, 1 - alpha / 2), type = 6, names = FALSE)
    quant_1_side <- quantile(est, probs = c(alpha, 1 - alpha), type = 6, names = FALSE)

    ci <- switch(alternative,
                 two.sided = quant_2_side,
                 greater = c(-Inf, quant_1_side[2]),
                 less = c(quant_1_side[1], Inf)
    )

    ret <- list(
        est = est_orig,  # First estimate should be original dataset
        ci = ci,
        se = NA,
        pvalue = min(pvals[index]) * length(pvals[index])
    )
    return(ret)
}


#' Title
#' @description
#' TODO
#' @param est TODO
#' @param conf.level TODO
#' @param alternative TODO
#' @importFrom stats sd qnorm pnorm quantile
pool_bootstrap_normal <- function(est, conf.level, alternative) {
    est_orig <- est[1] # First estimate should be original dataset
    est <- est[-1]
    alpha <- 1 - conf.level
    se <- sd(est)
    ret <- normal_ci(est_orig, se, alpha, alternative, qnorm, pnorm)
    return(ret)
}



#' Title
#'
#' @description
#' TODO
#' @param point TODO
#' @param se TODO
#' @param alpha TODO
#' @param alternative TODO
#' @param qfun TODO
#' @param pfun TODO
#' @param ... TODO
normal_ci <- function(point, se, alpha, alternative, qfun, pfun, ...) {
    ci <- switch(
        alternative,
        two.sided = c(-1, 1) * qfun(1 - alpha / 2, ...),
        greater =  c(-Inf, 1) *  qfun(1 - alpha, ...),
        less = c(-1, Inf) * qfun(1 - alpha, ...)
    ) * se + point

    pvals <- c(
        pfun(point / se, lower.tail = TRUE, ...),
        pfun(point / se, lower.tail = FALSE, ...)
    )

    index <- switch(
        alternative,
        two.sided = c(1, 2),
        greater = 2,
        less = 1
    )

    ret <- list(
        est = point,
        ci = ci,
        se = se,
        pvalue = min(pvals[index]) * length(pvals[index])
    )
    return(ret)
}





#' Transpose Results Object
#'
#' Transposes a Results object (as created by analyse()) inorder to group
#' the same estimates together into vectors.
#'
#' @param results TODO
#' @param components TODO
transpose_results <- function(results, components) {
    elements <- names(results[[1]])
    results_transpose <- list()
    for (element in elements) {
        results_transpose[[element]] <- list()
        for (comp in components) {
            results_transpose[[element]][[comp]] <- vapply(
                results,
                function(x) x[[element]][[comp]],
                numeric(1)
            )
        }
    }
    return(results_transpose)
}


#' Converts a pool object to a data.frame
#'
#' @param x (`pool`)\cr input
#' @param ... not used
#' @export
as.data.frame.pool <- function(x, ...) {
    data.frame(
        parameter = names(x$pars),
        est =vapply(x$pars, function(x) x$est, numeric(1)),
        se = vapply(x$pars, function(x) x$se, numeric(1)),
        lci = vapply(x$pars, function(x) x$ci[[1]], numeric(1)),
        uci = vapply(x$pars, function(x) x$ci[[2]], numeric(1)),
        pval = vapply(x$pars, function(x) x$pvalue, numeric(1)),
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}


#' Print Pool Object
#'
#' @param x (`pool`)\cr input
#' @param ... not used
#' @export
print.pool <- function(x, ...) {

    n_string <- ife(
        x$method == "rubin",
        as.character(x$N),
        sprintf("1 + %s", x$N - 1)
    )

    string <- c(
        "",
        "Pool Object",
        "-----------",
        sprintf("Number of Results Combined: %s", n_string),
        sprintf("Method: %s", x$method),
        sprintf("Confidence Level: %s", x$conf.level),
        sprintf("Alternative: %s", x$alternative),
        "",
        "Results:",
        as_ascii_table(as.data.frame(x), pcol = "pval"),
        ""
    )

    cat(string, sep = "\n")
    return(invisible(x))
}
