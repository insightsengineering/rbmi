remove_blank_spaces <- function(strings) {

    strings_nospaces <- gsub(
        ' ',
        '',
        strings
    )
    return(strings_nospaces)
}

#' @importFrom stats reformulate
designmat_to_formula <- function(
    designmat,
    outcome_var
) {

    formula <- stats::reformulate(
        termlabels = colnames(designmat[,-1, drop = FALSE]),
        response = outcome_var
    )

    return(formula)
}

random_effects_expr <- function(
    vars,
    names_groups,
    cov_struct = c("us", "toep", "cs", "ar1"),
    same_cov
) {
    # check for correct covariance structure specification (as per glmmTMB)
    match.arg(cov_struct)

    if(same_cov) {
        expr <- paste0(
            " + ",
            cov_struct,
            "(0 + ", vars$visit, " | ", vars$subjid, ")"
        )

    } else {
        expr <- ""
        for(name_group in names_groups) {
            expr <- paste(
                expr,
                paste0(cov_struct, "(0 + ", name_group, ":",  vars$visit, " | ", vars$subjid, ")"),
                sep = " + "
            )
        }
    }

    return(expr)
}

#' @importFrom stats as.formula
formula_mmrm <- function(
    designmat,
    vars,
    names_groups,
    cov_struct,
    same_cov
) {

    # fixed effects formula
    formula_fixeff <- designmat_to_formula(
        designmat = designmat,
        outcome_var = vars$outcome
    )

    # random effects for covariance structure
    expr_randeff <- random_effects_expr(
        vars = vars,
        names_groups = names_groups,
        cov_struct = cov_struct,
        same_cov = same_cov
    )

    # paste and create formula object
    formula <- as.formula(
        paste0(
            deparse(formula_fixeff, width.cutoff = 500),
            expr_randeff
        )
    )

    return(formula)
}

#' @importFrom glmmTMB fixef VarCorr getME
extract_params <- function(fit) {

    beta <- fixef(fit)$cond
    sigma <- VarCorr(fit)$cond
    theta <- getME(fit, name = "theta") # needed for initialization

    params <- list(
        beta = beta,
        sigma = sigma,
        theta = theta
    )

    return(params)
}

is_converged <- function(fit) {

    return(ifelse(fit$fit$convergence == 0, TRUE, FALSE))

}

#' @importFrom glmmTMB glmmTMB glmmTMBControl
#' @importFrom stats optim
fit_mmrm <- function(
    designmat,
    outcome,
    subjid,
    visit,
    group,
    vars,
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

    designmat <- as.data.frame(designmat)

    # remove blank spaces (otherwise formula cannot be built properly)
    colnames(designmat) <- remove_blank_spaces(colnames(designmat))
    vars <- lapply(
        vars,
        remove_blank_spaces
    )
    levels(group) <- remove_blank_spaces(levels(group))

    # create dummy variables for each arm (needed when same_cov = FALSE)
    groups_mat <- stats::model.matrix(~ 0 + group)
    colnames(groups_mat) <- levels(group)

    # build formula
    formula <- formula_mmrm(
        designmat = designmat,
        vars = vars,
        names_groups = colnames(groups_mat),
        cov_struct = cov_struct,
        same_cov = same_cov
    )

    # include in design matrix variables needed because included in the formula
    designmat_extended <- cbind(
        designmat,
        groups_mat,
        subjid,
        visit,
        outcome
    )

    colnames(designmat_extended) <- c(
        colnames(designmat),
        colnames(groups_mat),
        vars$subjid,
        vars$visit,
        vars$outcome
    )

    # set optimizer
    control <- glmmTMBControl(
        optimizer = optim,
        optArgs = list(method = optimizer),
        parallel = 1
    )

    # fit mmrm
    suppressWarnings({
        fit <- glmmTMB(
            formula,
            data = designmat_extended,
            dispformula = ~0,
            REML = REML,
            start = initial_values,
            control = control
        )
    })

    # extract regression coefficients and covariance matrices
    params <- extract_params(fit)

    # check convergence
    converged <- is_converged(fit)

    return_obj <- list(
        "beta" = params$beta,
        "sigma" = params$sigma,
        "theta" = params$theta,
        "converged" = converged
    )

    return(return_obj)
}

fit_mmrm_multiopt <- function(
    designmat,
    outcome,
    subjid,
    visit,
    group,
    vars,
    cov_struct = c("us", "toep", "cs", "ar1"),
    REML = TRUE,
    same_cov = TRUE,
    initial_values = NULL,
    optimizer = "L-BFGS-B"
) {

    converged <- FALSE
    iter <- 1
    while(!converged & iter <= length(optimizer)) {
        fit <- fit_mmrm(
            designmat,
            outcome,
            subjid,
            visit,
            group,
            vars,
            cov_struct,
            REML,
            same_cov,
            initial_values,
            optimizer = optimizer[iter]
        )
        if(fit$converged) {
            converged <- TRUE
            fit$optimizer <- optimizer[iter]
        }
        iter <- iter + 1
    }

    if(!converged) {
        fit$optimizer <- "None"
    }

    return(fit)
}
