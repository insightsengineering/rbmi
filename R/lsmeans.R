
#' Least Square Means
#'
#'
#' Estimates the least square means from a linear model. This is done by
#' generating a prediction from the model using an hypothetical observation
#' that is constructed by averaging the data. See details for more information.
#'
#' @details
#'
#' The lsmeans are obtained by calculating hypothetical patients
#' and predicting their expected values. These hypothetical patients
#' are constructed by expanding out all possible combinations of each
#' categorical covariate and by setting any numerical covariates equal
#' to the mean.
#'
#' A final lsmean value is calculated by averaging these hypothetical
#' patients. If `.weights` equals `"proportional"` then the values are weighted
#' by the frequency in which they occur in the full dataset. If `.weights`
#' equals `"equal"` then each hypothetical patient is given an equal weight
#' regardless of what actually occurs in the dataset.
#'
#' Use the `...` argument to fix specific variables to specific values.
#'
#' See the references for identical implementations as done in SAS and
#' in R via the `emmeans` package. This function attempts to re-implement the
#' `emmeans` derivation for standard linear models but without having to include
#' all of it's dependencies.
#'
#' @param model A model created by `lm`.
#' @param ... Fixes specific variables to specific values i.e.
#' `trt = 1` or `age = 50`. The name of the argument must be the name
#' of the variable within the dataset.
#' @param .weights Character, specifies whether to use "proportional" or "equal" weighting for each
#' categorical covariate combination when calculating the lsmeans.
#'
#' @references \url{https://CRAN.R-project.org/package=emmeans}
#' @references \url{https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.3/statug/statug_glm_details41.htm}
#' @examples
#' \dontrun{
#' mod <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' lsmeans(mod)
#' lsmeans(mod, Species = "virginica")
#' lsmeans(mod, Species = "versicolor")
#' lsmeans(mod, Species = "versicolor", Petal.Length = 1)
#' }
#' @importFrom stats model.matrix terms reformulate
lsmeans <- function(model, ..., .weights = c("proportional", "equal")) {
    .weights <- match.arg(arg = .weights)
    fix <- list(...)

    model_vars <- attr(terms(as.formula(model)), "term.labels")
    frm <- reformulate(model_vars)

    data <- char2fct(model$model)

    covars <- extract_covariates(model_vars)
    for (var in names(fix)) {
        assert_that(
            var %in% covars,
            msg = sprintf("Variable `%s` was not used in the model", var)
        )
    }

    design_fun <- switch(.weights,
        equal = ls_design_equal,
        proportional = ls_design_proportional
    )

    design <- design_fun(data, frm, covars, fix)
    beta <- matrix(coef(model), nrow = 1)
    list(
        est = as.vector(beta %*% design),
        se = as.vector(sqrt(t(design) %*% vcov(model) %*% design)),
        df = df.residual(model)
    )
}


#' Calculate design vector for the lsmeans
#'
#' Calculates the design vector as required to generate the lsmean
#' and standard error. `ls_design_equal` calculates it by
#' applying an equal weight per covariate combination whilst
#' `ls_design_proportional` applies weighting proportional
#' to the frequency in which the covariate combination occurred
#' in the actual dataset.
#'
#' @param data A data.frame
#' @param frm Formula used to fit the original model
#' @param covars a character vector of variables names that exist in
#' `data` which should be extracted (`ls_design_equal` only)
#' @param fix A named list of variables with fixed values
#' @name ls_design
ls_design_equal <- function(data, frm, covars, fix) {
    collection <- list()
    for (var in covars) {
        values <- data[[var]]
        if (is.factor(values)) {
            lvls <- levels(values)
            if (var %in% names(fix)) {
                collection[[var]] <- factor(fix[[var]], levels = lvls)
            } else {
                collection[[var]] <- factor(lvls, levels = lvls)
            }
        } else if (is.numeric(values)) {
            if (var %in% names(fix)) {
                collection[[var]] <- fix[[var]]
            } else {
                collection[[var]] <- mean(values)
            }
        } else {
            stop("invalid data type")
        }
    }
    design <- matrix(
        apply(model.matrix(frm, expand.grid(collection)), 2, mean),
        ncol = 1
    )
    return(design)
}


#' @rdname ls_design
ls_design_proportional <- function(data, frm, covars, fix) {
    data2 <- data
    for (var in names(fix)) {
        value <- data[[var]]
        if (is.numeric(value)) {
            data2[[var]] <- fix[[var]]
        } else if (is.factor(value)) {
            data2[[var]] <- factor(fix[[var]], levels = levels(data[[var]]))
        }
    }
    design <- colMeans(model.matrix(frm, data = data2))
    return(design)
}
