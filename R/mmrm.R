


#' Construct random effects formula - TODO DESCRIPTION
#'
#' Constructs a character representation of the random effects formula
#' for fitting a MMRM for subject by visit in the format required for glmmTMB.
#'
#' For example assuming the user specified a covariance structure of "us" and that no groups
#' were provided this will return
#'
#' ```
#' us(0 + visit | subjid)
#' ```
#'
#' If `group` is provided then this indicates that separate covariance matrices
#' are required per group and as such the following will be returned:
#'
#' ```
#' us( 0 + group1:visit | subjid) + us(0 + group2:visit | subjid) + ...
#' ```
#' @inheritParams fit_mmrm
random_effects_expr <- function(
    cov_struct = c("us", "toep", "cs", "ar1"),
    cov_by_group = FALSE
) {
    match.arg(cov_struct)
    if (cov_by_group) {
        frm <- sprintf("%s( visit | group / subjid)", cov_struct)
    } else {
        frm <- sprintf("%s( visit | subjid)", cov_struct)
    }
    return(frm)
}


#' Creates a "MMRM" ready dataset - TODO DOCUMENTATION
#'
#' Converts a design matrix + key variables into a command format
#' In particular this function does the following:
#' - Renames all covariates as `V1`, `V2`, etc to avoid issues of special characters in variable names
#' - Ensures all key variables are of the right type
#' - Inserts the outcome, visit and subjid variables into the `data.frame`
#' naming them as `outcome`, `visit` and `subjid`
#' - Splits a grouping variable out into separate columns, i.e. if `group` has 3 levels then the
#' output `data.frame` will have dummy indicator variables `G1`, `G2` & `G3`
#'
#' @inheritParams fit_mmrm
as_mmrm_df <- function(
    designmat,
    outcome,
    visit,
    subjid,
    group = NULL
) {

    if (length(group) == 0) group <- NULL

    dmat <- as.data.frame(designmat)
    colnames(dmat) <- paste0("V", seq_len(ncol(dmat)))

    assert_that(
        length(outcome) == nrow(dmat),
        length(visit) == nrow(dmat),
        length(subjid) == nrow(dmat),
        is.numeric(outcome),
        is.character(visit) | is.factor(visit),
        is.character(subjid) | is.factor(subjid),
        is.null(group) | is.character(group) | is.factor(group)
    )

    dmat[["outcome"]] <- outcome
    dmat[["visit"]] <- visit
    dmat[["subjid"]] <- subjid

    if (!is.null(group)) {
        dmat[["group"]] <- group
    }
    return(dmat)
}


#' Create MMRM formula - TODO Documentation
#'
#'
#' Derives the MMRM model formula from the structure of mmrm_df.
#' returns a formula object of the form:
#'
#' ```
#' outcome ~ 0 + V1 + V2 + V4 + ... + us(0 + group1:visit | subjid) + us(0 + group2:visit | subjid) + ...
#' ```
#' @param mmrm_df an mmrm `data.frame` as created by [as_mmrm_df()]
#' @inheritParams fit_mmrm
#' @importFrom stats as.formula
as_mmrm_formula <- function(mmrm_df, cov_struct) {
    dfnames <- names(mmrm_df)

    g_names <- grep("^group$", dfnames, value = TRUE)
    v_names <- grep("^V", dfnames, value = TRUE)

    assert_that(all(dfnames %in% c("outcome", "visit", "subjid", g_names, v_names)))
    assert_that(all(c("outcome", "visit", "subjid", g_names, v_names) %in% dfnames))

    # random effects for covariance structure
    expr_randeff <- random_effects_expr(
        cov_by_group = length(g_names) > 0,
        cov_struct = cov_struct
    )

    # paste and create formula object
    formula <- as.formula(
        sprintf("outcome ~ %s - 1", paste0(c(v_names, expr_randeff), collapse = " + "))
    )

    return(formula)
}


#' TODO - Documentation
#'
#' Extracts the beta and sigma coefficients from an MMRM model created
#' by glmmTMB-glmmTMB.
#' Also returns theta for use in providing initial values to subsequent calls.
#'
#' @param fit an object created by glmmTMB-glmmTMB
#'
extract_params <- function(fit) {
    beta <- coef(fit)
    names(beta) <- NULL

    sigma <- fit$cov
    if (!is.list(sigma)) {
        sigma <- list(sigma)
    }
    sigma <- lapply(sigma, function(mat) {
        colnames(mat) <- NULL
        rownames(mat) <- NULL
        return(mat)
    })

    params <- list(
        beta = beta,
        sigma = sigma
    )
    return(params)
}




#' Fit a MMRM model - TODO Documentation
#'
#' @description
#' Fits a MMRM model allowing for different covariance structures using glmmTMB-glmmTMB.
#' Returns a glmmTMB fit object with an additional element `failed` indicating whether or not
#' the fit failed to converge.
#'
#'
#' @param designmat a `data.frame` or `matrix` containing the covariates to use in the MMRM model.
#' Dummy variables must already be expanded out, i.e. via [stats::model.matrix()]. Cannot contain
#' any missing values
#' @param outcome a numeric vector. The outcome value to be regressed on in the MMRM model.
#' @param subjid a character / factor vector. The subject identifier used to link separate visits that belong to
#' the same subject.
#' @param visit a character / factor vector. Indicates which visit the outcome value occoured on.
#' @param group a character / factor vector. Indicates which treatment group the patient belongs to.
#' @param cov_struct a character value. Specifies which covariance structure to use. Must be one of
#' `"us"`, `"toep"`, `"cs"` or  `"ar1"`
#' @param REML logical. Specifies whether restricted maximum likelihood should be used
#' @param same_cov logical. Used to specify if a shared or individual covariance matrix should be used
#' per `group`
#' @param initial_values a list with names `beta` and `theta`. Specifies the initial values to start
#' the optimizer for glmmTMB-glmmTMB at.
#' @param optimizer a character value. Specifies the optimizer to be used in glmmTMB-glmmTMB. See
#' [stats::optim()] for the available options
#' @importFrom stats  model.matrix
#' @name fit_mmrm
#'
#'
fit_mmrm <- function(designmat,
                     outcome,
                     subjid,
                     visit,
                     group,
                     cov_struct = c("us", "toep", "cs", "ar1"),
                     REML = TRUE,
                     same_cov = TRUE) {
    dat_mmrm <- as_mmrm_df(
        designmat = designmat,
        outcome = outcome,
        visit = visit,
        subjid = subjid,
        group = ife(same_cov, NULL, group)
    )

    frm_mmrm <- as_mmrm_formula(dat_mmrm, cov_struct)

    fit <- eval_mmrm({
        mmrm::mmrm(
            formula = frm_mmrm,
            data = dat_mmrm,
            reml = REML,
            n_cores = 1,
            accept_singular = FALSE
        )
    })

    if (fit$failed) {
        return(fit)
    }

    # extract regression coefficients and covariance matrices
    params <- extract_params(fit)
    params$failed <- fit$failed

    # adjust covariance matrix
    if (same_cov) {
        assert_that(length(params$sigma) == 1)
        params$sigma <- replicate(
            length(levels(group)),
            params$sigma[[1]],
            simplify = FALSE
        )
    }
    names(params$sigma) <- levels(group) # TODO test this assumption is violated

    return(params)
}




#' Evaluate a call to mmrm
#'
#' This is a utility function that attempts to evaluate a call to mmrm
#' managing any warnings or errors that are thrown. In particular
#' this function attempts to catch any warnings or errors and instead
#' of surfacing them it will simply add an additional element `failed`
#' with a value of TRUE. This allows for multiple calls to be made
#' without the program exiting.
#'
#' This function was originally developed for use with glmmTMB which needed
#' more hand-holding and dropping of false-positive warnings. It is not
#' as important now but is kept around encase we need to catch
#' false-postive warnings again in the future.
#'
#' @examples
#' \dontrun{
#' eval_mmrm({
#'     mmrm::mmrm(formula, data)
#' })
#' }
#' @param expr An expression to be evaluated. Should be a call to [mmrm::mmrm()].
#' @seealso [record()]
#'
eval_mmrm <- function(expr) {

    default <- list(failed = TRUE)

    fit_record <- record(expr)

    if (length(fit_record$warnings) > 0 || length(fit_record$errors) > 0) {
        return(default)
    }

    converged <- attributes(fit_record$results)$converged
    if (is.null(converged)) {
        return(default)
    }
    if (!converged) {
        return(default)
    }

    fit_record$results$failed <- FALSE
    return(fit_record$results)
}
