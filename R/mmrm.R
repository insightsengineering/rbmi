


#' Construct random effects formula
#'
#' Constructs a character representation of the random effects formula
#' for fitting a MMRM for subject by visit in the format required for [mmrm::mmrm()].
#'
#' For example assuming the user specified a covariance structure of "us" and that no groups
#' were provided this will return
#'
#' ```
#' us(visit | subjid)
#' ```
#'
#' If `cov_by_group` is set to `FALSE` then this indicates that separate covariance matrices
#' are required per group and as such the following will be returned:
#'
#' ```
#' us( visit | group / subjid )
#' ```
#' @param cov_struct Character - The covariance structure to be used, must be one of `"us"`,
#' `"toep"`, `"cs"`, `"ar1"`
#' @param cov_by_group Boolean - Whenever or not to use separate covariances per each group level
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


#' Creates a "MMRM" ready dataset
#'
#' Converts a design matrix + key variables into a common format
#' In particular this function does the following:
#' - Renames all covariates as `V1`, `V2`, etc to avoid issues of special characters in variable names
#' - Ensures all key variables are of the right type
#' - Inserts the outcome, visit and subjid variables into the `data.frame`
#' naming them as `outcome`, `visit` and `subjid`
#' - If provided will also insert the group variable into the `data.frame` named as `group`
#'
#' @inheritParams fit_mmrm
as_mmrm_df <- function(designmat,
                       outcome,
                       visit,
                       subjid,
                       group = NULL) {
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


#' Create MMRM formula
#'
#'
#' Derives the MMRM model formula from the structure of mmrm_df.
#' returns a formula object of the form:
#'
#' ```
#' outcome ~ 0 + V1 + V2 + V4 + ... + us(visit | group / subjid)
#' ```
#' @param mmrm_df an mmrm `data.frame` as created by [as_mmrm_df()]
#' @param cov_struct Character - The covariance structure to be used, must be one of `"us"`,
#' `"toep"`, `"cs"`, `"ar1"`
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


#' Extract parameters from a MMRM model
#'
#' Extracts the beta and sigma coefficients from an MMRM model created
#' by [mmrm::mmrm()].
#'
#' @importFrom mmrm VarCorr
#' @param fit an object created by [mmrm::mmrm()]
#'
extract_params <- function(fit) {
    beta <- coef(fit)
    names(beta) <- NULL

    sigma <- VarCorr(fit)
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




#' Fit a MMRM model
#'
#' @description
#' Fits a MMRM model allowing for different covariance structures using [mmrm::mmrm()].
#' Returns a `list` of key model parameters `beta`, `sigma` and an additional element `failed`
#' indicating whether or not the fit failed to converge. If the fit did fail to converge
#' `beta` and `sigma` will not be present.
#'
#'
#' @param designmat a `data.frame` or `matrix` containing the covariates to use in the MMRM model.
#' Dummy variables must already be expanded out, i.e. via [stats::model.matrix()]. Cannot contain
#' any missing values
#' @param outcome a numeric vector. The outcome value to be regressed on in the MMRM model.
#' @param subjid a character / factor vector. The subject identifier used to link separate visits
#' that belong to the same subject.
#' @param visit a character / factor vector. Indicates which visit the outcome value occoured on.
#' @param group a character / factor vector. Indicates which treatment group the patient belongs to.
#' @param cov_struct a character value. Specifies which covariance structure to use. Must be one of
#' `"us"`, `"toep"`, `"cs"` or  `"ar1"`
#' @param REML logical. Specifies whether restricted maximum likelihood should be used
#' @param same_cov logical. Used to specify if a shared or individual covariance matrix should be used
#' per `group`
#' @param model_env an environment or NULL. If an environment is provided then the model fit object
#' will be recorded into it under `model_env$model`.
#' @name fit_mmrm
#'
fit_mmrm <- function(
    designmat,
    outcome,
    subjid,
    visit,
    group,
    cov_struct = c("us", "toep", "cs", "ar1"),
    REML = TRUE,
    same_cov = TRUE,
    model_env = NULL
) {
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
    
    # If the user provided an environment to record the model in then provide them the model
    if (is.environment(model_env)) {
        model_env$model <- fit
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
    names(params$sigma) <- levels(group)

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
