


#' Title
#'
#' @param cov_struct TODO
#' @param groups TODO
random_effects_expr <- function(
    cov_struct = c("us", "toep", "cs", "ar1"),
    groups = NULL
) {
    match.arg(cov_struct)
    if (length(groups) == 0) groups <- NULL
    grp_expr <- ife(is.null(groups), "", paste0(groups, ":"))
    expr <- paste0(
        sprintf("%s(0 + %svisit | subjid)", cov_struct, grp_expr),
        collapse = " + "
    )
    return(paste0(expr))
}


#' TODO
#' 
#' @param designmat TODO
#' @param outcome TODO
#' @param visit TODO
#' @param subjid TODO
#' @param groups TODO
as_mmrm_df <- function(
    designmat,
    outcome,
    visit,
    subjid,
    groups = NULL
) {
    if (length(groups) == 0) groups <- NULL

    dmat <- as.data.frame(designmat)
    colnames(dmat) <- paste0("V", seq_len(ncol(dmat)))

    assert_that(
        length(outcome) == nrow(dmat),
        length(visit) == nrow(dmat),
        length(subjid) == nrow(dmat),
        is.numeric(outcome),
        is.character(visit) | is.factor(visit),
        is.character(subjid) | is.factor(subjid),
        is.null(groups) | is.character(groups) | is.factor(groups)
    )

    dmat[["outcome"]] <- outcome
    dmat[["visit"]] <- visit
    dmat[["subjid"]] <- subjid

    if (!is.null(groups)) {
        # create dummy variables for each arm (needed when same_cov = FALSE)
        groups_mat <- stats::model.matrix(~ 0 + groups)
        for (i in 1:ncol(groups_mat)) {
            dmat[[paste0("G", i)]] <- groups_mat[, i]
        }
    }
    return(dmat)
}


#' Title
#'
#' @param mmrm_df TODO
#' @param cov_struct TODO
#' @importFrom stats as.formula
as_mmrm_formula <- function(mmrm_df, cov_struct) {

    dfnames <- names(mmrm_df)

    g_names <- grep("^G", dfnames, value = TRUE)
    v_names <- grep("^V", dfnames, value = TRUE)

    assert_that(all(dfnames %in% c("outcome", "visit", "subjid", g_names, v_names)))
    assert_that(all(c("outcome", "visit", "subjid", g_names, v_names) %in% dfnames))

    # random effects for covariance structure
    expr_randeff <- random_effects_expr(
        groups = g_names,
        cov_struct = cov_struct
    )

    # paste and create formula object
    formula <- as.formula(
        sprintf( "outcome ~ %s - 1", paste0(c(v_names, expr_randeff), collapse = " + "))
    )

    return(formula)
}


#' Title
#'
#' @param fit TODO
#' @importFrom glmmTMB fixef VarCorr getME
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




#' Title
#'
#' @param designmat TODO
#' @param outcome TODO
#' @param subjid TODO
#' @param visit TODO
#' @param group TODO
#' @param cov_struct TODO
#' @param REML TODO
#' @param same_cov TODO
#' @param initial_values TODO
#' @param optimizer TODO
#' @importFrom glmmTMB glmmTMB glmmTMBControl
#' @importFrom stats optim model.matrix
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
        groups = ife(same_cov, NULL, group)
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


#' Title
#'
#' @param optimizer TODO
#' @param ... TODO
fit_mmrm_multiopt <- function(..., optimizer) {

    assert_that(
        is.character(optimizer),
        length(optimizer) >= 1
    )

    for (opt in optimizer) {
        fit <- fit_mmrm(..., optimizer = opt)
        if (!fit$failed) break
    }

    return(fit)
}


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
