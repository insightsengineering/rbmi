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
    assertthat::assert_that(isSymmetric(res))
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


#
#         ** DEPRECIATED **
# please use get_sim_dat2 instead, this function is left here for compatibility with 
# tests that have already been defined based upon it
#
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
        dplyr::as_tibble() %>%
        dplyr::mutate(id = 1:n()) %>%
        tidyr::gather("visit", "outcome", -id) %>%
        dplyr::mutate(visit = factor(visit)) %>%
        dplyr::arrange(id, visit) %>%
        dplyr::left_join(covars, by = "id") %>%
        dplyr::mutate(outcome = outcome + 5 + 3 * age + 3 * f2n(sex) + trt * f2n(group)) %>%
        dplyr::mutate(id = as.factor(id))

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


is_nightly <- function() {
    Sys.getenv("R_TEST_NIGHTLY") == "TRUE"
}




# An enhanced version of get_sim_dat2
#
# - allows for custom coeficients
# - allows for treatment slope (instead of constant treatment)
# - allows for treatment slope to be a function of group and visit
# - allows for visit slope
# - returns a fixed n/2 per group (rather than randomly assigned)
# - Uses a proper character patient id rather than a character "1"
#
get_sim_dat2 <- function(
    n = 200,
    mcoefs = list("int" = 10, "age" = 3, "sex" = 2, "trt_slope" = 4, "visit_slope" = 2),
    sigma = as_covmat(c(3, 5, 7), c(0.1, 0.4, 0.7))
) {
    assert_that(n %% 2 == 0, msg = "n must be even")
    nv <- ncol(sigma)
    covars <- tibble::tibble(
        id = paste0("P", 1:n),
        age = rnorm(n),
        group = factor(rep(c("A", "B"), each = n / 2), levels = c("A", "B")),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F"))
    )

    trtslopefun <- ife(
        is.function(mcoefs[["trt_slope"]]),
        mcoefs[["trt_slope"]],
        function(grp, vis) mcoefs[["trt_slope"]] * grp * vis
    )

    dat <- mvtnorm::rmvnorm(n, sigma = sigma) %>%
        set_col_names(paste0("visit_", 1:nv)) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(id = paste0("P", 1:n)) %>%
        tidyr::gather("visit", "outcome", -id) %>%
        dplyr::mutate(visit = factor(visit)) %>%
        dplyr::arrange(id, visit) %>%
        dplyr::left_join(covars, by = "id") %>%
        dplyr::mutate(
            outcome = outcome +
                mcoefs[["int"]] +
                mcoefs[["age"]] * age +
                mcoefs[["sex"]] * f2n(sex) +
                mcoefs[["visit_slope"]] * f2n(visit) +
                trtslopefun(f2n(group), as.numeric(visit))
        ) %>%
        dplyr::mutate(id = as.factor(id))

    return(dat)
}