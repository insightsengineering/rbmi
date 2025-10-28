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


is_cluster_closed <- function(cl) {
    if (is.null(cl)) {
        return(TRUE)
    }
    if (!is(cl, "cluster")) {
        stop("`cl` is not a cluster object")
    }
    result <- tryCatch(
        {
            parallel::clusterCall(cl, function() Sys.info())
            FALSE
        },
        error = function(e) {
            TRUE
        }
    )
}


trunctate <- function(x, n) {
    floor(x * 10^n) / 10^n
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
        group = factor(
            sample(c("A", "B"), size = n, replace = TRUE),
            levels = c("A", "B")
        ),
        sex = factor(
            sample(c("M", "F"), size = n, replace = TRUE),
            levels = c("M", "F")
        )
    )

    dat <- mvtnorm::rmvnorm(n, sigma = sigma) %>%
        set_col_names(paste0("visit_", 1:nv)) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(id = seq_len(dplyr::n())) %>%
        tidyr::gather("visit", "outcome", -id) %>%
        dplyr::mutate(visit = factor(.data$visit)) %>%
        dplyr::arrange(id, .data$visit) %>%
        dplyr::left_join(covars, by = "id") %>%
        dplyr::mutate(
            outcome = .data$outcome +
                5 +
                3 * .data$age +
                3 * f2n(.data$sex) +
                trt * f2n(.data$group)
        ) %>%
        dplyr::mutate(id = as.factor(id))

    return(dat)
}


time_it <- function(expr) {
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

is_envvar_true <- function(var) {
    envvar <- Sys.getenv(var)
    if (is.null(envvar)) return(FALSE)
    if (is.na(envvar)) return(FALSE)
    if (toupper(envvar) %in% c("T", "TRUE")) return(TRUE)
    FALSE
}

is_local_test <- function() {
    is_envvar_true("R_TEST_LOCAL") || is_envvar_true("R_TEST_FULL")
}
is_full_test <- function() {
    is_envvar_true("R_TEST_FULL")
}

# Simple function to enable 1 function mocks
with_mocking <- function(expr, ..., where) {
    x <- list(...)
    nam <- names(x)
    fun <- x[[1]]
    assertthat::assert_that(length(x) == 1)
    hold <- get(nam, envir = where)
    islocked <- bindingIsLocked(nam, where)
    if (islocked) {
        unlockBinding(nam, where)
    }
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

ar1_matrix <- function(rho, n) {
    corr_matrix <- matrix(0, nrow = n, ncol = n)
    for (i in 1:n) {
        for (j in 1:n) {
            corr_matrix[i, j] <- rho^abs(i - j)
        }
    }
    corr_matrix
}

cs_matrix <- function(rho, n) {
    corr_matrix <- matrix(rho, nrow = n, ncol = n)
    diag(corr_matrix) <- 1
    corr_matrix
}

ad_matrix <- function(rhos) {
    n <- length(rhos) + 1
    corr_matrix <- matrix(0, nrow = n, ncol = n)
    for (i in 1:n) {
        for (j in i:n) {
            if (i == j) {
                corr_matrix[i, j] <- 1
            } else {
                corr_matrix[i, j] <- prod(rhos[i:(j - 1)])
                corr_matrix[j, i] <- corr_matrix[i, j]
            }
        }
    }
    corr_matrix
}

toep_matrix <- function(rhos) {
    n <- length(rhos) + 1
    corr_matrix <- matrix(0, nrow = n, ncol = n)
    for (i in 1:n) {
        for (j in 1:n) {
            corr_matrix[i, j] <- if (i == j) 1 else rhos[abs(i - j)]
        }
    }
    corr_matrix
}
