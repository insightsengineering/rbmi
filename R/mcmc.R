
#' Title - TODO
#'
#' @param designmat TODO
#' @param outcome TODO
#' @param group TODO
#' @param subjid TODO
#' @param visit TODO
#' @param same_cov TODO
#' @param n_imputations TODO
#' @param burn_in TODO
#' @param burn_between TODO
#' @param verbose TODO
#' @param seed TODO
#'
#' @import Rcpp
#' @import methods
#' @importFrom rstan sampling
#' @useDynLib rbmi, .registration = TRUE
fit_mcmc <- function(
    designmat,
    outcome,
    group,
    subjid,
    visit,
    same_cov,
    n_imputations,
    burn_in,
    burn_between,
    verbose = TRUE,
    seed = NULL
) {

    # fit MMRM (needed for initial values)
    mmrm_initial <- fit_mmrm_multiopt(
        designmat = designmat,
        outcome = outcome,
        subjid = subjid,
        visit = visit,
        group = group,
        cov_struct = "us",
        REML = TRUE,
        same_cov = same_cov,
        optimizer = c("L-BFGS-B", "BFGS")
    )

    stan_data <- prepare_stan_data(
        ddat = designmat,
        subjid = subjid,
        visit = visit,
        outcome = outcome,
        group = ife(same_cov == TRUE, rep(1, length(group)), group)
    )

    stan_data$Sigma_init <- ife(
        same_cov == TRUE,
        list(mmrm_initial$sigma[[1]]),
        mmrm_initial$sigma
    )

    sampling_args <- list(
        object = stanmodels$MMRM,
        data = stan_data,
        pars = c("beta", "Sigma"),
        chains = 1,
        warmup = burn_in,
        thin = burn_between,
        iter = burn_in + burn_between * n_imputations,
        init = list(list(
            theta = as.vector(stan_data$R %*% mmrm_initial$beta),
            sigma = mmrm_initial$sigma
        )),
        refresh = ife(
            verbose,
            (burn_in + burn_between * n_imputations) / 10,
            0
        )
    )

    sampling_args$seed <- seed

    stan_fit <- record({
        do.call(sampling, sampling_args)
    })

    if (!is.null(stan_fit$errors)) {
        stop(stan_fit$errors)
    }

    ignorable_warnings <- c(
        "Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.\nRunning the chains for more iterations may help. See\nhttp://mc-stan.org/misc/warnings.html#bulk-ess",
        "Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.\nRunning the chains for more iterations may help. See\nhttp://mc-stan.org/misc/warnings.html#tail-ess"
    )

    # handle warning: display only warnings if
    # 1) the warning is not in ignorable_warnings
    warnings <- stan_fit$warnings
    warnings_not_allowed <- warnings[!warnings %in% ignorable_warnings]
    for (i in warnings_not_allowed) warning(warnings_not_allowed)

    fit <- stan_fit$results
    check_mcmc(fit, n_imputations)

    draws <- extract_draws(fit)

    ret_obj <- list(
        "samples" = draws,
        "fit" = fit
    )

    return(ret_obj)
}




#' Transform array into list of arrays
#'
#' @description
#' Transform an array into list of arrays where the listing
#' is performed on a given dimension.
#'
#' @param a Array with number of dimensions at least 2.
#' @param n Positive integer. Dimension of `a` to be listed.
#'
#' @details
#' For example, if `a` is a 3 dimensional array and `n = 1`,
#' `split_dim(a,n)` returns a list of 2 dimensional arrays (i.e.
#' a list of matrices) where each element of the list is `a[i, , ]`, where
#' `i` takes values from 1 to the length of the first dimension of the array.
#'
#' @return
#' A list of `n-1` dimensional arrays.
#'
#' @examples
#' \dontrun{
#' set.seed(101)
#' a <- array(data = stats::rnorm(12), dim = c(3,2,2))
#' n <- 1
#' # creates a list of length 3 containing 2x2 matrices
#' split_a <- split_dim(a,n)
#' }
#'
#'
#' @importFrom stats setNames
split_dim <- function(a, n) {
    x <- split(
        a,
        arrayInd(seq_along(a), dim(a))[, n]
    )

    y <- lapply(
        x,
        array,
        dim = dim(a)[-n],
        dimnames = dimnames(a)[-n]
    )

    setNames(y, dimnames(a)[[n]])
}


#' Extract draws from a `stanfit` object
#'
#' @description
#' Extract draws from a `stanfit` object, manipulate it
#' and combine them together into a list.
#'
#' @param stan_fit A `stanfit` object.
#'
#' @return
#' A named list of length 2 containing:
#' - `beta`: a list of length equal to the number of draws containing
#'   the draws from the posterior distribution of the regression coefficients.
#' - `sigma`: a list of length equal to the number of draws containing
#'   the draws from the posterior distribution of the covariance matrices. Each element
#'   of the list is a list with length equal to 1 if `same_cov = TRUE` or equal to the
#'   number of groups if `same_cov = FALSE`.
#'
#' @importFrom rstan extract
extract_draws <- function(stan_fit) {

    pars <- extract(stan_fit, pars = c("beta", "Sigma"))
    names(pars) <- c("beta", "sigma")

    ##################### from array to list
    pars$sigma <- split_dim(pars$sigma, 1) # list of length equal to the number of draws

    pars$sigma <- lapply(
        pars$sigma,
        function(x) split_dim(x, 1)
    )

    pars$beta <- split_dim(pars$beta, 1)
    pars$beta <- lapply(pars$beta, as.vector)

    return(pars)
}


#' Extract the Effective Sample Size (ESS) from a `stanfit` object
#'
#' @param stan_fit A `stanfit` object.
#'
#' @return
#' A named vector containing the ESS for each parameter of the model.
#'
#' @importFrom rstan summary
get_ESS <- function(stan_fit) {
    return(rstan::summary(stan_fit, pars = c("beta", "Sigma"))$summary[, "n_eff"])
}


#' Diagnostics of the MCMC based on ESS
#'
#' @description
#' Check the quality of the MCMC draws from the posterior distribution
#' by checking whether the relative ESS is sufficiently large.
#'
#' @inheritParams check_mcmc
#'
#' @details
#' `check_ESS()` works as follows:
#' 1. Extract the ESS from `stan_fit` for each parameter of the model.
#' 2. Compute the relative ESS (i.e. the ESS divided by the number of draws).
#' 3. Check whether for any of the parameter the ESS is lower than `threshold`.
#'    If for at least one parameter the relative ESS is below the threshold,
#'    a warning is thrown.
#'
#' @inherit check_mcmc return
#'
check_ESS <- function(stan_fit, n_draws, threshold_lowESS = 0.4) {

    ESS <- get_ESS(stan_fit)

    n_low_ESS <- sum((ESS / n_draws) < threshold_lowESS)

    if (any((ESS / n_draws) < threshold_lowESS)) {
        warning(
            paste0(
                "The Effective Sample Size is below ",
                threshold_lowESS * 100,
                "% for ",
                n_low_ESS,
                " parameters. Please consider increasing burn-in and/or burn-between, or the number of samples"
            ),
            call. = FALSE
        )
    }

    return(invisible(NULL))
}


#' Diagnostics of the MCMC based on HMC-related measures.
#'
#' @description
#' Check that:
#' 1. There are no divergent iterations.
#' 2. The Bayesian Fraction of Missing Information (BFMI) is sufficiently low.
#' 3. The number of iterations that saturated the max treedepth is zero.
#'
#' Please see \link[rstan]{check_hmc_diagnostics} for details.
#'
#' @param stan_fit A `stanfit` object.
#'
#' @inherit check_mcmc return
#'
check_hmc_diagn <- function(stan_fit) {

    if (
        any(rstan::get_divergent_iterations(stan_fit)) | # draws "out of the distribution"
        isTRUE(rstan::get_bfmi(stan_fit) < 0.2) | # exploring well the target distribution
        any(rstan::get_max_treedepth_iterations(stan_fit)) # efficiency of the algorithm
    ) {
        warning(
            "Lack of efficiency in the HMC sampler: please consider increasing the burn-in period.",
            call. = FALSE
        )
    }

    return(invisible(NULL))
}


#' Diagnostics of the MCMC
#'
#' @param stan_fit A `stanfit` object.
#' @param n_draws Number of MCMC draws.
#' @param threshold_lowESS A number in `[0,1]` indicating the minimum acceptable
#' value of the relative ESS. See details.
#'
#' @details
#' Performs checks of the quality of the MCMC. See [check_ESS()] and [check_hmc_diagn()]
#' for details.
#'
#' @returns
#' A warning message in case of detected problems.
#'
check_mcmc <- function(stan_fit, n_draws, threshold_lowESS = 0.4) {

    check_ESS(
        stan_fit = stan_fit,
        n_draws = n_draws,
        threshold_lowESS = threshold_lowESS
    )

    check_hmc_diagn(stan_fit)

    return(invisible(NULL))
}



#' QR decomposition
#'
#' @description
#' QR decomposition as defined in the
#' \href{https://mc-stan.org/docs/2_27/stan-users-guide/QR-reparameterization-section.html}{Stan user's guide (section 1.2)}.
#'
#' @param mat A matrix to perform the QR decomposition on.
QR_decomp <- function(mat) {
    qr_obj <- qr(mat)
    N <- nrow(mat)
    Q <- qr.Q(qr = qr_obj) * sqrt(N - 1)
    R <- qr.R(qr = qr_obj) / sqrt(N - 1)

    ret_obj <- list(
        Q = Q,
        R = R
    )

    return(ret_obj)
}



#' Prepare input data to run the Stan model
#'
#' @description
#' Prepare input data to run the Stan model as per the `data{}` block of the related Stan file
#' where the model is implemented.
#'
#' @param ddat A design matrix
#' @param subjid Character vector containing the subjects IDs.
#' @param visit Vector containing the visits.
#' @param outcome Numeric vector containing the outcome variable.
#' @param group Vector containing the group variable.
#'
#' @returns
#' A `stan_data` object. A named list as per `data{}` block of the related Stan file.
#'
prepare_stan_data <- function(ddat, subjid, visit, outcome, group) {

    assert_that(
        is.factor(group) | is.numeric(group),
        is.factor(visit) | is.numeric(visit),
        is.character(subjid) | is.factor(subjid),
        is.numeric(outcome),
        is.data.frame(ddat) | is.matrix(ddat),
        length(group) == length(visit),
        length(subjid) == length(visit),
        length(outcome) == length(group),
        length(outcome) == nrow(ddat),
        length(unique(subjid)) * length(unique(visit)) == nrow(ddat)
    )

    design_variables <- paste0("V", seq_len(ncol(ddat)))
    ddat <- as.data.frame(ddat)
    names(ddat) <- design_variables
    ddat$subjid <- as.character(subjid)
    ddat$visit <- visit
    ddat$outcome <- outcome
    ddat$group <- group
    ddat$is_avail <- (!is.na(ddat$outcome)) * 1

    ddat <- remove_if_all_missing(ddat)

    dat_pgroups <- get_pattern_groups(ddat)

    ddat2 <- merge(ddat, dat_pgroups, by = "subjid", all = TRUE)
    ddat2 <- sort_by(ddat2, c("pgroup", "subjid", "visit"))
    assert_that(nrow(ddat2) == nrow(ddat))

    ddat3 <- ddat2[!is.na(ddat2$outcome), ]

    dmat <- as.matrix(ddat3[, design_variables])

    qr <- QR_decomp(dmat)

    dat_pgroups_u <- get_pattern_groups_unique(dat_pgroups)

    stan_dat <- list(
        N = nrow(dmat),
        P = ncol(dmat),
        G = length(unique(group)),
        n_visit = length(levels(visit)),
        n_pat = nrow(dat_pgroups_u),
        pat_G = as_stan_array(dat_pgroups_u$group_n),
        pat_n_pt = as_stan_array(dat_pgroups_u$n),
        pat_n_visit = as_stan_array(dat_pgroups_u$n_avail),
        pat_sigma_index = as_indices(dat_pgroups_u$pattern),
        y = ddat3$outcome,
        Q = qr$Q,
        R = qr$R
    )

    class(stan_dat) <- c("list", "stan_data")
    validate(stan_dat)
    return(stan_dat)
}


#' Title TODO
#'
#' Takes a dataset of pattern information and creates a summary dataset of it
#' with just 1 row per pattern
#'
#' @param patterns TODO
get_pattern_groups_unique <- function(patterns) {
    u_pats <- unique(patterns[, c("pgroup", "pattern", "group")])
    u_pats <- sort_by(u_pats, "pgroup")
    u_pats$group_n <- as.numeric(u_pats$group)
    u_pats$n <- as.numeric(tapply(patterns$pgroup, patterns$pgroup, length))
    u_pats$n_avail <- vapply(
        strsplit(u_pats$pattern, ""),
        function(x) sum(as.numeric(x)),
        numeric(1)
    )
    u_pats$group <- NULL
    return(u_pats)
}


#' Title - TODO
#'
#' Takes a design matrix with multiple rows per subject and returns a dataset
#' with 1 row per subject with a new column `pgroup` indicating which group
#' the patient belongs to (based upon their missingness pattern and treatment group)
#'
#' @param ddat TODO
get_pattern_groups <- function(ddat) {
    ddat <- sort_by(ddat, c("subjid", "visit"))[, c("subjid", "group", "is_avail")]

    pt_pattern <- tapply(ddat$is_avail, ddat$subjid, paste0, collapse = "")
    dat_pattern <- data.frame(
        subjid = names(pt_pattern),
        pattern = pt_pattern,
        stringsAsFactors = FALSE,
        row.names = NULL
    )

    dat_group <- unique(ddat[, c("subjid", "group")])

    assert_that(
        nrow(dat_group) == length(unique(ddat$subjid)),
        nrow(dat_group) == nrow(dat_pattern),
        all(ddat$subjid %in% dat_group$subjid)
    )

    dat_pgroups <- merge(dat_group, dat_pattern, all = TRUE, by = "subjid")
    dat_pgroups$pgroup <- as_strata(dat_pgroups$pattern, dat_pgroups$group)
    return(dat_pgroups)
}


#' TODO
#'
#' Converts a string of 0's and 1's into index positions of the 1's
#' padding the results by 0's so they are all the same length
#'
#' i.e. patmap(c("1101", "0001"))  ->   list(c(1,2,4,0), c(4,0,0,0))
#'
#' @param x TODO
as_indices <- function(x) {

    assert_that(
        length(unique(nchar(x))) == 1,
        msg = "all values of x must be the same length"
    )

    len <- max(nchar(x))
    lapply(
        strsplit(x, ""),
        function(x) {
            assert_that(
                all(x %in% c("0", "1")),
                msg = "All values of x must be 0 or 1"
            )
            temp <- rep(0, len)
            y <- which(x == "1")
            temp[seq_along(y)] <- y
            return(temp)
        }
    )
}

#' Title - TODO
#'
#' @param x TODO
as_stan_array <- function(x) {
    ife(
        length(x) == 1,
        array(x, dim = 1),
        x
    )
}


#' Title TODO
#'
#' @param dat TODO
remove_if_all_missing <- function(dat) {
    n_visit <- length(unique(dat$visit))
    n_miss <- tapply(dat$outcome, dat$subjid, function(x) sum(is.na(x)))
    remove_me <- Filter(function(x) x == n_visit, n_miss)
    remove_me_pt <- names(remove_me)
    dat[!dat$subjid %in% remove_me_pt, ]
}


#' Validate a `stan_data` object
#'
#' @param x A `stan_data` object.
#' @param ... Not used.
#'
#' @export
validate.stan_data <- function(x, ...) {
    assert_that(
        x$N == nrow(x$Q),
        x$P == ncol(x$Q),
        sum(x$pat_n_visit * x$pat_n_pt) == nrow(x$Q),
        ncol(x$Q) == ncol(x$R),
        ncol(x$R) == nrow(x$R),
        length(x$y) == nrow(x$Q),
        length(x$pat_G) == length(x$pat_n_pt),
        length(x$pat_G) == length(x$pat_n_visit),
        length(x$pat_G) == length(x$pat_sigma_index),
        length(unique(lapply(x$pat_sigma_index, length))) == 1,
        length(x$pat_sigma_index[[1]]) == x$n_visit,
        all(vapply(x$pat_sigma_index, function(z) all(z %in% c(seq_len(x$n_visit), 0)), logical(1)))
    )
}
