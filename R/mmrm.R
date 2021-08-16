
#' Title
#'
#' @param strings TODO
#' @param chr TODO
remove_character <- function(strings, chr) {

    assert_that(
        is.character(chr),
        msg = "chr must be a character"
    )

    strings_nospaces <- gsub(
        chr,
        '',
        strings
    )

    return(strings_nospaces)
}

#' Title
#'
#' @param designmat TODO
#' @param outcome_var TODO
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

#' Title
#'
#' @param vars TODO
#' @param names_groups TODO
#' @param cov_struct TODO
#' @param same_cov TODO
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

#' Title
#'
#' @param designmat TODO
#' @param vars TODO
#' @param names_groups TODO
#' @param cov_struct TODO
#' @param same_cov TODO
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

#' Title
#'
#' @param fit TODO
#' @importFrom glmmTMB fixef VarCorr getME
extract_params <- function(fit) {

    beta <- fixef(fit)$cond
    sigma <- VarCorr(fit)$cond
    sigma <- lapply(sigma, function(x) as.matrix(data.frame(x))) # remove all attributes
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
#' @param vars TODO
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
    colnames(designmat) <- remove_character(colnames(designmat), " ")
    colnames(designmat) <- remove_character(colnames(designmat), ":")
    vars <- lapply(
        vars,
        function(x) remove_character(x, " ")
    )
    levels(group) <- remove_character(levels(group), " ")

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

    ###################### fit mmrm



    fit <- record_warnings({
        glmmTMB(
            formula,
            data = designmat_extended,
            dispformula = ~0,
            REML = REML,
            start = initial_values,
            control = control
        )
    })

    # check convergence
    converged <- is_converged(fit$results)

    ignorable_warnings <- c(
        "'giveCsparse' has been deprecated; setting 'repr = \"T\"' for you"
    )

    # handle warning: display only warnings if
    # 1) fit does converge
    # 2) the warning is not in ignorable_warnings
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
    if(same_cov) {
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
#' @param designmat TODO
#' @param outcome TODO
#' @param subjid TODO
#' @param visit TODO
#' @param group TODO
#' @param vars TODO
#' @param cov_struct TODO
#' @param REML TODO
#' @param same_cov TODO
#' @param initial_values TODO
#' @param optimizer TODO
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





