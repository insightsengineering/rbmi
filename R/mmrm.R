


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

    assert_that( all(dfnames %in% c("outcome", "visit", "subjid", g_names, v_names)))

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
#' @param fit TODO
is_converged <- function(fit) {
    return(ifelse(fit$fit$convergence == 0, TRUE, FALSE))
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

    # set optimizer
    control <- glmmTMBControl(
        optimizer = optim,
        optArgs = list(method = optimizer),
        parallel = 1
    )

    ###################### fit mmrm
    fit <- record_warnings({
        glmmTMB(
            formula = frm_mmrm,
            data = dat_mmrm,
            dispformula = ~0,
            REML = REML,
            start = initial_values,
            control = control
        )
    })


    # check convergence
    converged <- is_converged(fit$results)



    # handle warning: display only warnings if
    # 1) fit does converge
    # 2) the warning is not in ignorable_warnings
    ignorable_warnings <- c(
        "OpenMP not supported.",
        "'giveCsparse' has been deprecated; setting 'repr = \"T\"' for you"
    )
    if (converged) {
        warnings <- fit$warnings
        warnings_to_ignore <- str_contains(warnings, ignorable_warnings)
        warnings_to_raise <- warnings[!warnings_to_ignore]
        for (i in warnings_to_raise) warning(i)
    }

    fit <- fit$results

    # extract regression coefficients and covariance matrices
    params <- extract_params(fit)

    # adjust covariance matrix
    if (same_cov) {
        params$sigma <- list(params$sigma[[1]], params$sigma[[1]])
    }
    names(params$sigma) <- levels(group)

    return_obj <- list(
        "beta" = params$beta,
        "sigma" = params$sigma,
        "theta" = params$theta,
        "converged" = converged
    )

    return(return_obj)
}


#' Title
#'
#' @param optimizer TODO
#' @param ... TODO
fit_mmrm_multiopt <- function(..., optimizer = "L-BFGS-B") {

    converged <- FALSE
    iter <- 1
    while(!converged & iter <= length(optimizer)) {
        opti <- optimizer[iter]
        fit <- fit_mmrm(..., optimizer = opti)
        if(fit$converged) {
            converged <- TRUE
            fit$optimizer <- opti
        }
        iter <- iter + 1
    }

    if (!converged) {
        fit$optimizer <- "None"
    }

    return(fit)
}





