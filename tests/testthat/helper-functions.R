set_col_names <- function(x, nam) {
    colnames(x) <- nam
    return(x)
}

f2n <- function(x) as.numeric(x) - 1

as_covmat <- function(sig, corr) {
    len <- length(sig)
    cormat <- diag(rep(1, len))
    index <- 1
    for (i in 1:len) {
        for (j in 1:len) {
            if (i < j) {
                cormat[i, j] <- corr[index]
                cormat[j, i] <- corr[index]
                index <- index + 1
            }
        }
    }
    return((sig %*% t(sig)) * cormat)
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


get_sim_data <- function(n, sigma){
    nv <- ncol(sigma)
    covars <- tibble(
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
        mutate(outcome = outcome + 5 + 3 * age + 3 * f2n(sex) + 4 * f2n(group)) %>%
        mutate(id = as.character(id))

    return(dat)
}

time_it <- function(expr){
    start <- Sys.time()
    expr
    stop <- Sys.time()
    difftime(stop, start, units = "secs")
}