


#' Create simulated datasets
#'
#' @description
#' Creates a longitudinal dataset in the format that `rbmi` was
#' designed to analyse.
#'
#' @details
#' The number of visits is determined by the size of the variance covariance matrix.
#' i.e. if 3 standard deviation values are provided then 3 visits per patient will be
#' created.
#'
#' The covariates in the simulated dataset are produced as follows:
#' - Patients age is sampled at random from a N(0,1) distribution
#' - Patients sex is sampled at random with a 50/50 split
#' - Patients group is sampled at random but fixed so that each group has `n/2` patients
#' - The outcome variable is sampled from a multivariate normal distribution, see below
#' for details
#'
#' The mean for the outcome variable is derived as:
#' ```
#' outcome = Intercept + age + sex + visit + treatment
#' ```
#' The coefficients for the intercept, age and sex are taken from `mu$int`,
#' `mu$age` and `mu$sex` respectively, all of which must be a length 1 numeric.
#'
#' Treatment and visit coefficients are taken from `mu$trt` and `mu$visit` respectively
#' and must either be of length 1 (i.e. a constant affect across all visits) or equal to the
#' number of visits (as determined by the length of `sd`). I.e. if you wanted a treatment
#' slope of 5 and a visit slope of 1 you could specify:
#' ```
#' mu = list(..., "trt" = c(0,5,10), "visit" = c(0,1,2))
#' ```
#'
#' The correlation matrix is constructed from `cor` as follows.
#' Let `cor = c(a, b, c, d, e, f)` then the correlation matrix would be:
#' ```
#' 1  a  b  d
#' a  1  c  e
#' b  c  1  f
#' d  e  f  1
#' ```
#'
#' @param n the number of subjects to sample. Total number of observations returned
#' is thus `n * length(sd)`
#' @param sd the standard deviations for the outcome at each visit.
#' i.e. the square root of the diagonal of the covariance matrix for the outcome
#' @param cor the correlation coefficients between the outcome values at each visit.
#' See details.
#' @param mu the coefficients to use to construct the mean outcome value at each visit. Must
#' be a named list with elements `int`, `age`, `sex`, `trt` & `visit`. See details.
#'
#' @name simulate_test_data
simulate_test_data <- function(
    n = 200,
    sd = c(3, 5, 7),
    cor = c(0.1, 0.7, 0.4),
    mu = list("int" = 10, "age" = 3, "sex" = 2, "trt" = c(0, 4, 8), "visit" = c(0, 1, 2))
) {

    nv <- length(sd)
    assert_that(
        n %% 2 == 0,
        msg = "n must be even"
    )
    num_dig <- floor(log(x = n, base = 10)) + 1
    num_dig_vis <- floor(log(x = nv, base = 10)) + 1

    n_cor <- sum(seq_len(length(sd) - 1))
    assert_that(
        length(sd) == n_cor,
        msg = sprintf("There should be %s correlation parameters", n_cor)
    )

    assert_that(
        length(mu$trt) %in% c(1, nv),
        msg = sprintf("`mu$trt` must be of length 1 or %s", nv)
    )

    assert_that(
        length(mu$visit) %in% c(1, nv),
        msg = sprintf("`mu$trt` must be of length 1 or %s", nv)
    )

    pt_ids <- sprintf(paste0("P%0", num_dig, "d"), seq_len(n))
    vis_ids <- sprintf(paste0("visit_%0", num_dig_vis, "d"), 1:nv)
    sigma <- as_vcov(sd, cor)

    covars <- data.frame(
        id = pt_ids,
        age = rnorm(n),
        group = factor(rep(c("A", "B"), each = n / 2), levels = c("A", "B")),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F")),
        stringsAsFactors = FALSE
    )

    samps <- replicate(
        n = n,
        sample_mvnorm(rep(0, nv), sigma),
        simplify = FALSE
    )
    samps_mat <- matrix(unlist(samps), nrow = n, byrow = TRUE)
    colnames(samps_mat) <- vis_ids

    samps_dat <- as.data.frame(samps_mat)
    samps_dat$id <- pt_ids

    dat <- Reduce(rbind, lapply(vis_ids, function(vis) {
        x <- samps_dat[, c("id", vis)]
        colnames(x) <- c("id", "outcome")
        x[["visit"]] <- vis
        as.data.frame(x)
    }))

    dat$visit <- factor(dat$visit)
    dat <- sort_by(dat, c("id", "visit"))

    dat2 <- merge(dat, covars, by = "id")
    dat2 <- sort_by(dat2, c("id", "visit", "group"))

    assert_that(
        nrow(dat) == nrow(dat2),
        ncol(dat2) == ncol(dat) + 3
    )

    f2n <- function(x) as.numeric(x) - 1

    dat2$id <- factor(dat2$id, levels = pt_ids)
    dat2$outcome <- dat2$outcome +
        mu[["int"]] +
        mu[["age"]] * dat2$age +
        mu[["sex"]] * f2n(dat2$sex) +
        rep(mu[["visit"]], length.out = nrow(dat2)) +
        rep(mu[["trt"]], length.out = nrow(dat2)) * f2n(dat2$group)

    return(dat2)
}



#' @rdname simulate_test_data
#' @export
as_vcov <- function(sd, cor) {
    x <- diag(rep(1, length(sd)))
    x[upper.tri(x)] <- cor
    x <- t(x)
    x[upper.tri(x)] <- cor
    res <- diag(sd) %*% x %*% diag(sd)
    res <- as.matrix(Matrix::nearPD(res)$mat)
    assertthat::assert_that(isSymmetric(res))
    dimnames(res) <- NULL
    return(res)
}
