
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
#' @param initial_values TODO
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
    verbose,
    seed
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
        initial_values = NULL,
        optimizer = c("L-BFGS-B", "BFGS")
    )

    stan_data <- prepare_stan_data(
        ddat = designmat,
        id = id,
        visit = visit,
        outcome = outcome,
        group = ife(same_cov == TRUE, rep(1, length(group)), group)
    )
    stan_data$Sigma_init <- mmrm_initial$sigma

    # set verbose (if verbose = TRUE then refresh is set to default value)
    refresh <- ife(
        verbose,
        (burn_in + burn_between * n_imputations) / 10,
        0
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
            beta = mmrm_initial$beta,
            sigma = mmrm_initial$sigma
        )),
        refresh = refresh
    )

    if (!is.na(seed)) {
        sampling_args$seed <- seed
    }

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




#' Title - TODO
#'
#' @param a TODO
#' @param n TODO
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


#' Title - TODO
#'
#' @param stan_fit TODO
#' @importFrom rstan extract
extract_draws <- function(stan_fit) {

    pars <- extract(stan_fit, pars = c("beta", "Sigma"))
    names(pars) <- c("beta", "sigma")

    ##################### from array to list
    pars$sigma <- split_dim(pars$sigma, 1)

    if (length(dim(pars$sigma[[1]])) == 3) { # if same_cov == FALSE
        pars$sigma <- lapply(
            pars$sigma,
            function(x) split_dim(x, 1)
        )
    } else {
        pars$sigma <- lapply(
            pars$sigma,
            function(x) list(x, x)
        )
    }

    pars$beta <- split_dim(pars$beta, 1)
    pars$beta <- lapply(pars$beta, as.vector)

    return(pars)
}


#' Title - TODO
#'
#' @param stan_fit TODO
#' @importFrom rstan summary
get_ESS <- function(stan_fit) {
    return(rstan::summary(stan_fit, pars = c("beta", "Sigma"))$summary[, "n_eff"])
}


#' Title - TODO
#'
#' @param stan_fit TODO
#' @param n_draws TODO
#' @param threshold TODO
check_ESS <- function(stan_fit, n_draws, threshold = 0.4) {

    ESS <- get_ESS(stan_fit)

    n_low_ESS <- sum((ESS / n_draws) < threshold)

    if (any((ESS / n_draws) < threshold)) {
        warning(
            paste0(
                "The Effective Sample Size is below ",
                threshold * 100,
                "% for ",
                n_low_ESS,
                " parameters. Please consider increasing burn-in and/or burn-between, or the number of samples"
            ),
            call. = FALSE
        )
    }

    return(invisible(NULL))
}


#' Title - TODO
#'
#' @param stan_fit TODO
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


#' Title - TODO
#'
#' @param stan_fit TODO
#' @param n_draws TODO
#' @param threshold_lowESS TODO
check_mcmc <- function(stan_fit, n_draws, threshold_lowESS = 0.4) {

    check_ESS(
        stan_fit = stan_fit,
        n_draws = n_draws,
        threshold = threshold_lowESS
    )

    check_hmc_diagn(stan_fit)

    return(invisible(NULL))
}




#' Title - TODO
#'
#' @param ddat TODO
#' @param id TODO
#' @param visit TODO
#' @param outcome TODO
#' @param group TODO
prepare_stan_data <- function(ddat, id, visit, outcome, group) {

    assert_that(
        is.factor(group) | is.numeric(group),
        is.factor(visit) | is.numeric(visit),
        is.character(id),
        is.numeric(outcome),
        is.data.frame(ddat),
        length(group) == length(visit),
        length(id) == length(visit),
        length(outcome) == length(group),
        length(outcome) == nrow(ddat),
        length(unique(id)) * length(unique(visit)) == nrow(ddat)
    )

    design_variables <- paste0("V", seq_len(ncol(ddat)))
    names(ddat) <- design_variables
    ddat$id <- id
    ddat$visit <- visit
    ddat$outcome <- outcome
    ddat$group <- group
    ddat$is_avail <- (!is.na(ddat$outcome)) * 1

    dat_pgroups <- get_pattern_groups(ddat)

    ddat2 <- merge(ddat, dat_pgroups, by = "id", all = TRUE)
    ddat2 <- sort_by(ddat2, c("pgroup", "id", "visit"))
    assert_that(nrow(ddat2) == nrow(ddat))

    ddat3 <- ddat2[!is.na(ddat2$outcome), ]

    dmat <- as.matrix(ddat3[, design_variables])

    dat_pgroups_u <- get_pattern_groups_unique(dat_pgroups)

    assert_that(
        sum(dat_pgroups_u$n_avail * dat_pgroups_u$n) == nrow(dmat)
    )

    stan_dat <- list(
        N = nrow(dmat),
        P = ncol(dmat),
        G = length(unique(group)),
        n_visit = length(levels(visit)),
        n_pat = nrow(dat_pgroups_u),
        pat_G = dat_pgroups_u$group_n,
        pat_n_pt = dat_pgroups_u$n,
        pat_n_visit = dat_pgroups_u$n_avail,
        pat_sigma_index = as_indices(dat_pgroups_u$pattern),
        y = ddat3$outcome,
        X = dmat
    )
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
    u_pats$n <- tapply(patterns$pgroup, patterns$pgroup, length)
    u_pats$n_avail <- vapply(
        strsplit(u_pats$pattern, ""),
        function(x) sum(as.numeric(x)),
        numeric(1)
    )
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
    ddat <- sort_by(ddat, c("id", "visit"))

    pt_pattern <- tapply(ddat$is_avail, ddat$id, paste0, collapse = "")
    dat_pattern <- data.frame(
        id = names(pt_pattern),
        pattern = pt_pattern,
        stringsAsFactors = FALSE,
        row.names = NULL
    )

    dat_group <- unique(ddat[, c("id", "group")])

    assert_that(
        nrow(dat_group) == length(unique(ddat$id)),
        nrow(dat_group) == nrow(dat_pattern),
        all(metadat$id %in% dat_group$id)
    )

    dat_pgroups <- merge(dat_group, dat_pattern, all = TRUE, by = "id")
    dat_pgroups$pgroup <- as_strata(dat_pgroups$pattern, dat_pgroups$group)
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
    len <- max(nchar(x))
    lapply(
        strsplit(x, ""),
        function(x) {
            temp <- rep(0, len)
            y <- which(x == "1")
            temp[seq_along(y)] <- y
            return(temp)
        }
    )
}