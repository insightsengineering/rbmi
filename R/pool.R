








transpose_results <- function(results){
    elements <- names(results[[1]])
    results_transpose <- list()

    for( element in elements){
        results_transpose[[element]] = list(
            est = vapply(results, function(x) x[[element]][["est"]], numeric(1)),
            se = vapply(results, function(x) x[[element]][["se"]], numeric(1)),
            df = vapply(results, function(x) x[[element]][["df"]], numeric(1))
        )
    }
    return(results_transpose)
}




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
){
    alternative <- match.arg(alternative)
    validate_analysis_results(results)
    results_transpose <- transpose_results(results)
    ret <- lapply(
        results_transpose, 
        pool_,
        conf.level,
        alternative
    )
    return(ret)
}


#' @rdname pool
#' @export
pool_ <- function(results, conf.level, alternative, ...){
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
    
    
    ci <- switch(
        alternative,
        two.sided = c(-1, 1) * qnorm(1 - alpha / 2),
        greater =  c(-Inf, 1) *  qnorm(1 - alpha),
        less = c(-1, Inf) * qnorm(1- alpha)
    ) * se_jack + est_point
    
    
    pvals <- c(
        pnorm(est_point, sd = se_jack, lower.tail = TRUE),
        pnorm(est_point, sd = se_jack, lower.tail = FALSE)
    )
    
    index <- switch(
        alternative,
        two.sided = c(1, 2),
        greater = 1,
        less = 2
    )
    
    ret <- list(
        est = est_point,
        ci = ci,
        pvalue = min(pvals[index]) * length(pvals[index])
    )
    return(ret)
    
}





pool_bootstrap_percentile <- function(est, conf.level, alternative){
    alpha <- 1 - conf.level
    pvals <- (c(sum(est < 0),sum(est > 0)) + 1) / (length(est) + 1)
    
    index <- switch(
        alternative,
        two.sided = c(1, 2),
        greater = 1,
        less = 2
    )
    
    ret <- list(
        est = est[1],  # First estimate should be original dataset
        ci = quantile(est, probs = c(alpha / 2, 1 - alpha / 2), type = 6, names = FALSE),
        pvalue = min(pvals[index]) * length(pvals[index])
    )
    return(ret)
}




pool_bootstrap_normal <- function(est, conf.level, alternative) {
    alpha <- 1 - conf.level
    est_point <- est[1] # First estimate should be original dataset
    se <- sd(est)
    
    ci <- switch(
        alternative,
        two.sided = c(-1, 1) * qnorm(1 - alpha / 2),
        greater =  c(-Inf, 1) *  qnorm(1 - alpha),
        less = c(-1, Inf) * qnorm(1- alpha)
    ) * se + est_point
    
    pvals <- c(
        pnorm(est_point, sd = se, lower.tail = TRUE),
        pnorm(est_point, sd = se, lower.tail = FALSE)
    )
    
    index <- switch(
        alternative,
        two.sided = c(1, 2),
        greater = 1,
        less = 2
    )
    
    ret <- list(
        est = est_point,
        ci = ci,
        pvalue = min(pvals[index]) * length(pvals[index])
    )
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
){
    type = match.arg(type)
    bootfun <- switch(
        type,
        percentile = pool_bootstrap_percentile,
        normal = pool_bootstrap_normal
    )
    
    x <- bootfun(results$est, conf.level, alternative)
    return(x)
}




#' @rdname pool
#' @export
pool_.rubin <- function(results, ...) {
    ests <- results$est
    ses <- results$se
    dfs <- results$df
    est_point <- mean(ests)
    var_w <- mean(ses^2)
    var_b <- var(ests)
    
    var_r <- var_w + (1 + 1 / length(ests)) * var_b
    
}

