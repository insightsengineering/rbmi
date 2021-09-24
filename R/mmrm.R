


#' Construct random effects formula
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
    group = NULL
) {
    match.arg(cov_struct)
    if (length(group) == 0) group <- NULL
    ugroup <- unique(group)
    grp_expr <- ife(is.null(ugroup), "", paste0(ugroup, ":"))
    expr <- paste0(
        sprintf("%s(0 + %svisit | subjid)", cov_struct, grp_expr),
        collapse = " + "
    )
    return(paste0(expr))
}


#' Creates a "MMRM" ready dataset
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
        # create dummy variables for each arm (needed when same_cov = FALSE)
        group_mat <- stats::model.matrix(~ 0 + group)
        for (i in 1:ncol(group_mat)) {
            dmat[[paste0("G", i)]] <- group_mat[, i]
        }
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
#' outcome ~ 0 + V1 + V2 + V4 + ... + us(0 + group1:visit | subjid) + us(0 + group2:visit | subjid) + ...
#' ```
#' @param mmrm_df an mmrm `data.frame` as created by [as_mmrm_df()]
#' @inheritParams fit_mmrm
#' @importFrom stats as.formula
as_mmrm_formula <- function(mmrm_df, cov_struct) {

    dfnames <- names(mmrm_df)

    g_names <- grep("^G", dfnames, value = TRUE)
    v_names <- grep("^V", dfnames, value = TRUE)

    assert_that(all(dfnames %in% c("outcome", "visit", "subjid", g_names, v_names)))
    assert_that(all(c("outcome", "visit", "subjid", g_names, v_names) %in% dfnames))

    # random effects for covariance structure
    expr_randeff <- random_effects_expr(
        group = g_names,
        cov_struct = cov_struct
    )

    # paste and create formula object
    formula <- as.formula(
        sprintf( "outcome ~ %s - 1", paste0(c(v_names, expr_randeff), collapse = " + "))
    )

    return(formula)
}


#' Extract glmmTMB model parameters
#'
#' Extracts the beta and sigma coefficients from an MMRM model created
#' by [glmmTMB::glmmTMB()].
#' Also returns theta for use in providing initial values to subsequent calls.
#'
#' @param fit an object created by [glmmTMB::glmmTMB()]
#' @importFrom glmmTMB fixef VarCorr getME
#'
extract_params <- function(fit) {
    beta <- fixef(fit)$cond
    names(beta) <- NULL

    sigma <- VarCorr(fit)$cond
    sigma <- lapply(sigma, function(x) {
        x <- as.matrix(as.data.frame(x))
        colnames(x) <- NULL
        rownames(x) <- NULL
        return(x)
    })

    theta <- getME(fit, name = "theta") # needed for initialization

    params <- list(
        beta = beta,
        sigma = sigma,
        theta = theta
    )
    return(params)
}




#' Fit a MMRM model
#'
#' @description
#' Fits a MMRM model allowing for different covariance structures using [glmmTMB::glmmTMB()].
#' Returns a glmmTMB fit object with an additional element `failed` indicating whether or not
#' the fit failed to converge.
#'
#'
#' @param designmat a `data.frame` or `matrix` containing the covariates to use in the MMRM model.
#' dummy variables must already be expanded out I.e. via [stats::model.matrix()]. Cannot contain
#' any missing values
#' @param outcome a numeric vector. The outcome value to be reggressed on in the mmrm model.
#' @param subjid a character / factor vector. The subject identify use to link separate visits that belong to
#' the same subject.
#' @param visit a character / factor vector. Indicates which visit the outcome value occoured on.
#' @param group a character / factor vector. Used to indicate which treatment group the patient belongs to.
#' @param cov_struct a character value. Specifies which covariance structure to use. Must be one of
#' `"us"`, `"toep"`, `"cs"` or  `"ar1"`
#' @param REML local. Specifies whether restricted maximium likelihood should be used
#' @param same_cov logical. Used to specify if a shared or individual covariance matrix should be used
#' per `group`
#' @param initial_values a list with names `beta` and `theta`. Specifies the initial values to start
#' the optimizer for [glmmTMB::glmmTMB()] at.
#' @param optimizer a character value. Specifies the optimizer to be used in [glmmTMB::glmmTMB()]. See
#' [stats::optim()] for the available options
#' @importFrom glmmTMB glmmTMB glmmTMBControl
#' @importFrom stats optim model.matrix
#' @name fit_mmrm
#'
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
    initial_values = NULL,
    optimizer = "L-BFGS-B"
) {

    # check that optimizer is one among the optimizers from optim
    match.arg(
        arg = optimizer,
        choices = formals(fun = stats::optim)$method
    )

    dat_mmrm <- as_mmrm_df(
        designmat = designmat,
        outcome = outcome,
        visit = visit,
        subjid = subjid,
        group = ife(same_cov, NULL, group)
    )

    frm_mmrm <- as_mmrm_formula(dat_mmrm, cov_struct)

    fit <- eval_glmmtmb({
        glmmTMB(
            formula = frm_mmrm,
            data = dat_mmrm,
            dispformula = ~0,
            REML = REML,
            start = initial_values,
            control = glmmTMBControl(
                optimizer = optim,
                optArgs = list(method = optimizer),
                parallel = 1
            )
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
    names(params$sigma) <- levels(group)

    return(params)
}



#' Fit an mmrm model via multiple optimizers
#'
#'
#' The function attempts to fit a mmrm model using the optimizer as specified in `optimizer`
#' If `optimizer` is of length > 1 then it will loop through all the values until one of them is able
#' to converge. That is to say if a fit fails to converge it will move onto the next value of `optimizer` and
#' try again.
#'
#' If `optimizer` is a list then the names of the list with taken to be the required `optimizer`
#' with the contents of that element being used as the initial values.  This functionality
#' can be used to try and fit the model using the same optimizer at multiple different starting
#' values e.g.:
#'
#' ```
#' fit_mmrm_multiopt(
#'     ...,
#'     optimizer = list(
#'         "L-BFGS-B" = list(beta = c(1,2,3), theta = c(9,8,7)),
#'         "L-BFGS-B" = list(beta = c(5,6,7), theta = c(10,11,12)),
#'     )
#' )
#' ```
#'
#' See [stats::optim()] for a list of the available optimizers that can be used
#' @param optimizer A character vector or a named list. See details.
#' @param ... Additional arguments passed onto [fit_mmrm()]
#' @seealso [fit_mmrm()]
fit_mmrm_multiopt <- function(..., optimizer) {

    assert_that(
        is.character(optimizer) | is.list(optimizer),
        length(optimizer) >= 1
    )

    if (is.list(optimizer)) {
        for (i in seq_len(length(optimizer))) {
            if (!is.null(optimizer[[i]])) {
                assert_that(
                    all(c("beta", "theta") %in% names(optimizer[[i]]))
                )
                optimizer[[i]] <- optimizer[[i]][c("beta", "theta")]
            }
        }
    }

    if (is.character(optimizer)) {
        initial_values <- lapply(optimizer, function(x) NULL)
        names(initial_values) <- optimizer
    } else {
        initial_values <- optimizer
    }

    for (i in seq_len(length(initial_values))) {
        opt <- names(initial_values)[[i]]
        init_vals <- initial_values[[i]]
        fit <- fit_mmrm(..., initial_values = init_vals, optimizer = opt)
        if (!fit$failed) break
    }

    return(fit)
}


#' Evaluate a call to glmmTMB
#'
#' This is a utility function that attempts to evaluate a call to glmmTMB
#' managing any warnings or errors that are thrown. In particular
#' this function attempts to catch any warnings or errors and instead
#' of surfacing them it will simply add an additional element `failed`
#' with a value of TRUE.  This allows for multiple calls to be made
#' without the program exiting. Additionally this function will suppress
#' known spurious warnings associated with glmmTMB, namely:
#'
#' - https://stackoverflow.com/questions/67040472/warning-in-every-model-of-glmmtmb-givecsparse
#'
#' @examples
#' \dontrun{
#'     eval_glmmtmb({glmmTMB::glmmTMB(formula, data)})
#' }
#' @param expr An expression to be evaluated. Should be a call to [glmmTMB::glmmTMB()].
#' @seealso [glmmTMB::glmmTMB()]
#' @seealso [record()]
#'
eval_glmmtmb <- function(expr) {

    default <- list(failed = TRUE)

       ###################### fit mmrm
    fit_record <- record(expr)

    ignorable_warnings <- c(
        "OpenMP not supported.",
        "'giveCsparse' has been deprecated; setting 'repr = \"T\"' for you"
    )
    warnings <- fit_record$warnings
    warnings_to_ignore <- str_contains(warnings, ignorable_warnings)
    fit_record$warnings <- warnings[!warnings_to_ignore]


    if (length(fit_record$warnings) > 0  | length(fit_record$errors) > 0) {
        return(default)
    }

    if (fit_record$results$fit$convergence != 0) {
        return(default)
    }

    fit_record$results$failed <- FALSE
    return(fit_record$results)
}
