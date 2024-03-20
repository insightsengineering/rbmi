
#' Least Square Means
#'
#'
#' Estimates the least square means from a linear model. The exact implementation
#' / interpretation depends on the weighting scheme; see details for more information.
#'
#' @details
#'
#' For `.weights = "proportional"` (the default) the lsmeans are obtained by
#' taking the average of the fitted values for each patient.
#' In terms of the interactions between two continuous covariates 
#' this is equivalent to constructing a hypothetical patient whose
#' interaction term is `mean(X * Y)`. This is opposed to the
#' `emmeans` package which calculates the interaction term
#' of the hypothetical patient as `mean(X) * mean(Y)`. The approach
#' outlined here is equivalent to standardization or g-computation.
#' 
#' For `.weights = "equal"` the lsmeans are obtained by taking the model fitted
#' value of a hypothetical patient whose covariates are defined as follows:
#' - Continuous covariates are set to `mean(X)`
#' - Dummy categorical variables are set to `1/N` where `N` is the number of levels
#' - Continuous * continuous interactions are set to `mean(X) * mean(Y)`
#' - Continuous * categorical interactions are set to `mean(X) * 1/N`
#'
#' Regardless of the weighting scheme any named arguments passed via `...` will
#' fix the value of the covariate to the specified value.
#' For example, `lsmeans(model, trt = "A")` will fix the dummy variable `trtA` to 1
#' for all patients (real or hypothetical) when calculating the lsmeans.
#'
#' See the references for similar implementations as done in SAS and
#' in R via the `emmeans` package.
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

    design <- design_fun(data, frm, fix)
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
#' @param fix A named list of variables with fixed values
#' @name ls_design
ls_design_equal <- function(data, frm, fix) {
    assert_that(
        inherits(frm, "formula"),
        is.data.frame(data),
        is.list(fix),
        all(names(fix) %in% colnames(data))
    )
    frm2 <- update(frm, NULL ~ .)
    data2 <- model.frame(frm2, data)
    collection <- lapply(as.list(data2), collapse_values)
    
    for (var in names(fix)) {
        if (is.numeric(data2[[var]])) {
            collection[[var]] <- fix[[var]]
        } else if (is.factor(data2[[var]])) {
            collection[[var]] <- factor(fix[[var]], levels = levels(data2[[var]]))
        }
    }

    all_combinations <- expand.grid(collection)

    design_matrix <- model.matrix(frm2, all_combinations)

    result <- matrix(
        apply(design_matrix, 2, mean),
        ncol = 1
    )
    return(result)
}


collapse_values <- function(x) {
    UseMethod("collapse_values")
}

#' @export
collapse_values.factor <- function(x) {
    return(factor(levels(x), levels(x)))
}

#' @export
collapse_values.numeric <- function(x) {
    return(mean(x))
}

#' @export
collapse_values.default <- function(x) {
    stop("invalid data type")
}


#' @rdname ls_design
ls_design_proportional <- function(data, frm, fix) {
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
