#' Prepare input data to run the Stan model for count outcome
#'
#' @description
#' Prepare input data to run the Stan model for count outcome.
#'
#' @param ddat A design matrix
#' @param subjid A vector of subject IDs
#' @param period A vector of period values
#' @param duration A vector of duration values
#' @param outcome A vector of integer outcome values (unscaled, i.e. raw counts)
#' @param group A factor containing the treatment group for each row.
#' @param same_cov Logical. If `TRUE`, use one shared dispersion parameter. If
#'   `FALSE`, estimate a separate dispersion parameter for each treatment group.
#'
#' @return A `stan_data_count` object. A named list containing all the
#' required inputs as required by the `data{}` block of the count Stan program:
#'
#' - `N`: The number of patients
#' - `R`: The number of observed positive-duration cells
#' - `P`: The number of design matrix columns in each period
#' - `G`: The number of dispersion parameter groups
#' - `group`: The dispersion parameter group index for each patient
#' - `subject`: The patient index for each observed cell
#' - `y`: The observed cell counts
#' - `X`: The cell-level design matrix
#' - `log_offset`: The log exposure for each observed cell
#'
#' @keywords internal
prepare_stan_data_count <- function(
    ddat,
    subjid,
    period,
    duration,
    outcome,
    group,
    same_cov
) {
    assert_that(
        is.factor(period) | is.character(period) | is.numeric(period),
        is.numeric(duration) & all(duration >= 0),
        is.character(subjid) | is.factor(subjid),
        is.numeric(outcome) &
            all((outcome == trunc(outcome) & (outcome >= 0)) | is.na(outcome)),
        is.data.frame(ddat) | is.matrix(ddat),
        length(period) == length(duration),
        length(duration) == length(outcome),
        length(outcome) == length(subjid),
        length(group) == length(subjid),
        is.factor(group),
        is.logical(same_cov) & length(same_cov) == 1 & !is.na(same_cov),
        nrow(ddat) == length(subjid),
        all(is.finite(as.matrix(ddat)))
    )

    subject_ids <- unique(as.character(subjid))
    subject_all <- match(as.character(subjid), subject_ids)
    observed <- !is.na(outcome) & duration > 0
    assert_that(
        all(outcome[duration == 0 & !is.na(outcome)] == 0),
        all(tabulate(subject_all[observed], nbins = length(subject_ids)) > 0),
        msg = paste(
            "Each subject must have at least one observed positive-duration",
            "count cell, and zero-duration cells cannot have positive counts"
        )
    )

    ddat <- as.matrix(ddat[observed, , drop = FALSE])
    subjid_observed <- subject_all[observed]
    outcome <- outcome[observed]
    duration <- duration[observed]

    N <- length(subject_ids)
    R <- length(outcome)
    P <- ncol(ddat)
    G <- ife(same_cov, 1L, nlevels(group))
    group_integer <- as.integer(group)
    group_is_constant <- vapply(
        split(group_integer, subjid),
        function(x) length(unique(x)) == 1,
        logical(1)
    )
    assert_that(
        all(group_is_constant),
        msg = "Treatment group must be constant within subject"
    )
    group_by_subject <- ife(
        same_cov,
        rep(1L, N),
        vapply(
            seq_len(N),
            function(i) unique(group_integer[subject_all == i]),
            integer(1)
        )
    )
    assert_that(
        length(group_by_subject) == N,
        all(group_is_constant),
        msg = "Treatment group must be constant within subject"
    )

    stan_dat <- list(
        N = N,
        R = R,
        P = P,
        G = G,
        group = group_by_subject,
        subject = subjid_observed,
        y = as.integer(outcome),
        X = ddat,
        log_offset = log(duration)
    )

    class(stan_dat) <- c("list", "stan_data", "stan_data_count")
    validate(stan_dat)
    stan_dat
}


#' Validate a `stan_data_count` object
#'
#' @param x A `stan_data_count` object.
#' @param ... Not used.
#' @export
validate.stan_data_count <- function(x, ...) {
    assert_that(
        x$R == length(x$y),
        x$R == length(x$log_offset),
        x$R == nrow(x$X),
        x$R == length(x$subject),
        length(x$group) == x$N,
        x$P == ncol(x$X),
        x$N >= 1,
        x$R >= 1,
        x$P >= 1,
        is.numeric(x$G),
        length(x$G) == 1,
        x$G >= 1,
        all(x$group == trunc(x$group)),
        all(x$group >= 1 & x$group <= x$G),
        all(x$subject == trunc(x$subject)),
        all(x$subject >= 1 & x$subject <= x$N),
        !anyNA(x$y),
        all(x$y == trunc(x$y)),
        all(x$y >= 0),
        all(is.finite(x$X)),
        all(is.finite(x$log_offset)),
        msg = "Invalid Stan Data Object for Count Outcome"
    )
}


#' Complete Stan control options for count outcomes
#'
#' @param control A named list of arguments passed to [rstan::sampling()].
#' @param n_samples Number of retained posterior draws requested by the user.
#' @param quiet Logical indicating whether Stan progress output is suppressed.
#' @param stan_data A `stan_data_count` object. Currently unused, but retained
#'   for a common control-completion interface across outcome types.
#'
#' @return The completed `control` list, including calculated `iter` and
#'   `refresh` values and a count-compatible initialization setting.
#'
#' @keywords internal
complete_control_bayes_count <- function(
    control,
    n_samples,
    quiet,
    stan_data
) {
    control <- complete_control_bayes_common(
        control = control,
        n_samples = n_samples,
        quiet = quiet
    )

    control$init <- "random"
    control
}

#' Compile the Stan model for count outcomes
#'
#' @return An `rstan::stanmodel` for the ragged negative-multinomial count
#'   likelihood.
#'
#' @keywords internal
get_stan_model_count <- function() {
    # TODO: This should all go in general function

    # Compiling Stan models updates the current seed state. This can lead to
    # non-reproducibility as compiling is conditional on wether there is a cached
    # model available or not. Thus we save the current seed state and restore it
    # at the end of this function so that it is in the same state regardless of
    # whether the model was compiled or not.
    # See https://github.com/openpharma/rbmi/issues/469
    # Note that .Random.seed is only set if the seed has been set or if a random number
    # has been generated.
    current_seed_state <- globalenv()$.Random.seed
    on.exit({
        if (
            is.null(current_seed_state) &&
                exists(".Random.seed", envir = globalenv())
        ) {
            rm(".Random.seed", envir = globalenv(), inherits = FALSE)
        } else {
            assign(
                ".Random.seed",
                value = current_seed_state,
                envir = globalenv(),
                inherits = FALSE
            )
        }
    })

    ensure_rstan()

    # Find the correct Stan file for count outcome.
    file_loc_count_model <- find_stan_file(
        "count_model.stan"
    )
    model_template <- jinjar::parse_template(
        fs::path(file_loc_count_model),
        .config = jinjar::jinjar_config(
            trim_blocks = TRUE,
            lstrip_blocks = TRUE
        )
    )
    model_string <- jinjar::render(model_template)

    model_name <- "rbmi_count_model"

    # TODO: Enable caching but we need some general function for that

    model <- rstan::stan_model(
        model_code = model_string,
        model_name = model_name,
        auto_write = FALSE,
        save_dso = FALSE
    )

    model
}


#' Extract draws from a Stan fit object for count outcome
#'
#' @param stan_fit An `rstan::stanfit` object produced by the count model.
#' @param n_samples Number of posterior draws to retain.
#'
#' @return A named list with `beta` and `phi` components. Each component is a
#'   list with one element per retained posterior draw.
#'
#' @keywords internal
extract_draws_count <- function(stan_fit, n_samples) {
    assertthat::assert_that(assertthat::is.number(n_samples))

    pars <- rstan::extract(stan_fit, pars = c("beta", "phi"))
    names(pars) <- c("beta", "phi")

    pars$beta <- split_dim(pars$beta, 1)
    pars$beta <- lapply(pars$beta, as.vector)
    assertthat::assert_that(length(pars$beta) >= n_samples)
    pars$beta <- pars$beta[seq_len(n_samples)]

    pars$phi <- if (is.null(dim(pars$phi))) {
        lapply(pars$phi, function(x) x)
    } else {
        lapply(split_dim(pars$phi, 1), as.vector)
    }
    assertthat::assert_that(length(pars$phi) >= n_samples)
    pars$phi <- pars$phi[seq_len(n_samples)]

    return(pars)
}
