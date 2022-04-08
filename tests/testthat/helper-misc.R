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
# please use `simulate_test_data` instead, this function is left here for compatibility with
# tests that have already been defined based upon it
#
get_sim_data <- function(n, sigma, trt = 4) {
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
        dplyr::mutate(id = 1:dplyr::n()) %>%
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
    Sys.getenv("R_TEST_FULL") == "TRUE"
}



# Simple function to enable 1 function mocks
with_mocking <- function(expr, ..., where) {
    x <- list(...)
    nam <- names(x)
    fun <- x[[1]]
    assertthat::assert_that(length(x) == 1)
    hold <- get(nam, envir = where)
    islocked <- bindingIsLocked(nam, where)
    if (islocked) unlockBinding(nam, where)
    assign(nam, fun, envir = where)

    x <- tryCatch(
        expr,
        finally = {
            assign(nam, hold, where)
            if (islocked) lockBinding(nam, where)
        }
    )
    return(x)
}

