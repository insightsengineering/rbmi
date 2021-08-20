suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})

set_col_names <- function(x, nam) {
    colnames(x) <- nam
    return(x)
}

f2n <- function(x) as.numeric(x) - 1



as_covmat <- function(sig, corr) {
    x <- diag(rep(1, length(sig)))
    x[upper.tri(x)] <- corr
    x <- t(x)
    x[upper.tri(x)] <- corr
    res <- diag(sig) %*% x %*% diag(sig)
    res <- as.matrix(Matrix::nearPD(res)$mat)
    assert_that(isSymmetric(res))
    return(res)
}



strip_names <- function(x) {
    names(x) <- NULL
    colnames(x) <- NULL
    rownames(x) <- NULL
    return(x)
}

trunctate <- function(x, n) {
    floor(x * 10 ^ n) / 10 ^ n
}


get_sim_data <- function(n, sigma, trt = 4){
    nv <- ncol(sigma)
    covars <- tibble::tibble(
        id = 1:n,
        age = rnorm(n),
        group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F"))
    )

    dat <- mvtnorm::rmvnorm(n, sigma = sigma) %>%
        set_col_names(paste0("visit_", 1:nv)) %>%
        as_tibble() %>%
        mutate(id = 1:n()) %>%
        tidyr::gather("visit", "outcome", -id) %>%
        mutate(visit = factor(visit)) %>%
        arrange(id, visit) %>%
        left_join(covars, by = "id") %>%
        mutate(outcome = outcome + 5 + 3 * age + 3 * f2n(sex) + trt * f2n(group)) %>%
        mutate(id = as.factor(id))

    return(dat)
}

time_it <- function(expr){
    start <- Sys.time()
    expr
    stop <- Sys.time()
    difftime(stop, start, units = "secs")
}



expect_within <- function(x, bounds) {
    expect_gt(x, bounds[1])
    expect_lt(x, bounds[2])
}


expect_contains <- function(x, y) {
    expect_within(y, x)
}


is_nightly <- function(){
    Sys.getenv("R_TEST_NIGHTLY") == "TRUE"
}
