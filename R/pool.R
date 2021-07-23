

#' TODO
#'
#' @name pool
#' @param results TODO
#' @param ... TODO
#'
#' @export
pool <- function(
    results,
    conf.level = 0.95,
    alternative = c("two.sided", "less", "greater"),
    ...
) {

    alternative <- match.arg(alternative)

    assert_that(
        is.numeric(conf.level),
        length(conf.level) == 1,
        0 < conf.level & conf.level < 1,
        msg = "`conf.level` must be between 0 and 1"
    )

    validate_analysis_results(results)
    results_transpose <- transpose_results(results)
    pars <- lapply(
        results_transpose,
        getS3method("pool_", class(results)),
        conf.level,
        alternative,
        ...
    )
    ret <- list(
        pars = pars,
        conf.level = conf.level,
        alternative = alternative
    )
    return(ret)
}


#' @rdname pool
#' @export
pool_ <- function(results, conf.level, alternative, ...) {
    UseMethod("pool_")
}



#' @rdname pool
#' @export
pool_.jackknife <- function(results, conf.level, alternative, ...){
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
    type = c("percentile", "normal"),
    ...
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




#' @rdname pool
#' @export
pool_.rubin <- function(results, conf.level, alternative, ...) {
    ests <- results$est
    ses <- results$se
    dfs <- results$df
    alpha <- 1 - conf.level

    M <- length(ests)
    est_point <- mean(ests)

    var_w <- mean(ses^2)
    var_b <- var(ests)
    var_t <- var_w + (1 + 1 / M) * var_b

    v_com <- unique(dfs)

    assert_that(
        length(v_com) == 1,
        msg = "Degrees of freedom should be consistent across all samples"
    )

    if (is.na(v_com) | is.infinite(v_com)) {
        df <- Inf
    } else {
        lambda <- (1 + 1 / M) * var_b / var_t
        v_obs <- ((v_com + 1) / (v_com + 3)) * v_com * (1 - lambda)
        v_old <- (M - 1)  / lambda^2
        df <- (v_old * v_obs) / (v_old + v_obs)
    }

    ret <- normal_ci(
        point = est_point,
        se = sqrt(var_t),
        alpha = alpha,
        alternative = alternative,
        qfun = function(p, ...) qt(p, df = df),
        pfun = function(q, lower.tail, ...) pt(q, df = df, lower.tail = lower.tail)
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
        est = est[1],  # First estimate should be original dataset
        ci = ci,
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
pool_bootstrap_normal <- function(est, conf.level, alternative) {
    alpha <- 1 - conf.level
    est_point <- est[1] # First estimate should be original dataset
    se <- sd(est)
    ret <- normal_ci(est_point, se, alpha, alternative, qnorm, pnorm)
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
normal_ci <- function(point, se, alpha, alternative, qfun, pfun) {
    ci <- switch(
        alternative,
        two.sided = c(-1, 1) * qfun(1 - alpha / 2),
        greater =  c(-Inf, 1) *  qfun(1 - alpha),
        less = c(-1, Inf) * qfun(1 - alpha)
    ) * se + point

    pvals <- c(
        pfun(point, sd = se, lower.tail = TRUE),
        pfun(point, sd = se, lower.tail = FALSE)
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
transpose_results <- function(results) {
    elements <- names(results[[1]])
    results_transpose <- list()

    for (element in elements) {
        results_transpose[[element]] <- list(
            est = vapply(results, function(x) x[[element]][["est"]], numeric(1)),
            se = vapply(results, function(x) x[[element]][["se"]], numeric(1)),
            df = vapply(results, function(x) x[[element]][["df"]], numeric(1))
        )
    }
    return(results_transpose)
}
