
#' Least Square Means
#'
#'
#' Estimates the least square means from a linear model. The exact implementation
#' / interpretation depends on the weighting scheme; see the weighting section for more
#' information.
#'
#'
#' @section Weighting:
#'
#' ### Counterfactual
#'
#' For `weights = "counterfactual"` (the default) the lsmeans are obtained by
#' taking the average of the predicted values for each patient after assigning all patients
#' to each arm in turn.
#' This approach is equivalent to standardization or g-computation.
#' In comparison to `emmeans` this approach is equivalent to:
#' ```
#' emmeans::emmeans(model, specs = "<treatment>", counterfactual = "<treatment>")
#' ```
#' Note that to ensure backwards compatibility with previous versions of `rbmi`
#' `weights = "proportional"` is an alias for `weights = "counterfactual"`.
#' To get results consistent with `emmeans`'s `weights = "proportional"`
#' please use `weights = "proportional_em"`.
#'
#' ### Equal
#'
#' For `weights = "equal"` the lsmeans are obtained by taking the model fitted
#' value of a hypothetical patient whose covariates are defined as follows:
#' - Continuous covariates are set to `mean(X)`
#' - Dummy categorical variables are set to `1/N` where `N` is the number of levels
#' - Continuous * continuous interactions are set to `mean(X) * mean(Y)`
#' - Continuous * categorical interactions are set to `mean(X) * 1/N`
#' - Dummy categorical * categorical interactions are set to `1/N * 1/M`
#'
#' In comparison to `emmeans` this approach is equivalent to:
#' ```
#' emmeans::emmeans(model, specs = "<treatment>", weights = "equal")
#' ```
#'
#' ### Proportional
#'
#' For `weights = "proportional_em"` the lsmeans are obtained as per `weights = "equal"`
#' except instead of weighting each observation equally they are weighted by the proportion
#' in which the given combination of categorical values occurred in the data.
#' In comparison to `emmeans` this approach is equivalent to:
#' ```
#' emmeans::emmeans(model, specs = "<treatment>", weights = "proportional")
#' ```
#' Note that this is not to be confused with `weights = "proportional"` which is an alias
#' for `weights = "counterfactual"`.
#'
#' @section Fixing:
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
#' @param .weights Character, either `"counterfactual"` (default), `"equal"`,
#' `"proportional_em"` or `"proportional"`.
#' Specifies the weighting strategy to be used when calculating the lsmeans.
#' See the weighting section for more details.
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
lsmeans <- function(
    model,
    ...,
    .weights = c("counterfactual", "equal", "proportional_em", "proportional")
) {
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

    if (.weights == "proportional") {
        message(
            paste(
                "NOTE: The `proportional` weighting scheme is an alias for `counterfactual`",
                "and will be deprecated in the future. Please use `proportional_em` or",
                "`counterfactual` as appropriate instead.",
                sep = " "
            )
        )
    }

    design_fun <- switch(.weights,
        counterfactual = ls_design_counterfactual,
        proportional = ls_design_counterfactual,
        equal = ls_design_equal,
        proportional_em = ls_design_proportional
    )

    design <- design_fun(data, frm, fix)
    beta <- matrix(coef(model), nrow = 1)
    list(
        est = as.vector(beta %*% design),
        se = as.vector(sqrt(t(design) %*% vcov(model) %*% design)),
        df = df.residual(model)
    )
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
#' @importFrom stats update model.frame aggregate
#' @name ls_design
ls_design_equal <- function(data, frm, fix) {
    assert_that(
        inherits(frm, "formula"),
        is.data.frame(data),
        is.list(fix),
        all(names(fix) %in% colnames(data)),
        all(vapply(fix, length, numeric(1)) == 1)
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


#' @rdname ls_design
ls_design_counterfactual <- function(data, frm, fix) {
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


#' @rdname ls_design
ls_design_proportional <- function(data, frm, fix) {
    assert_that(
        inherits(frm, "formula"),
        is.data.frame(data),
        is.list(fix),
        all(names(fix) %in% colnames(data)),
        all(vapply(fix, length, numeric(1)) == 1)
    )
    frm2 <- update(frm, NULL ~ .)
    dat2 <- model.frame(frm2, data)
    collection <- lapply(as.list(dat2), collapse_values)

    for (var in names(fix)) {
        if (is.numeric(dat2[[var]])) {
            dat2[[var]] <- fix[[var]]
            collection[[var]] <- fix[[var]]
        } else if (is.factor(dat2[[var]])) {
            dat2[[var]] <- factor(fix[[var]], levels = levels(dat2[[var]]))
            collection[[var]] <- factor(fix[[var]], levels = levels(dat2[[var]]))
        }
    }

    all_combinations <- expand.grid(collection)
    design_matrix <- model.matrix(frm2, all_combinations)

    categorical_vars_fl <- vapply(
        dat2,
        function(x) is.character(x) || is.factor(x), logical(1)
    )
    categorical_vars <- names(which(categorical_vars_fl))

    wgts <- aggregate(
        dat2[, categorical_vars[[1]]],
        as.list(dat2[, categorical_vars, drop = FALSE]),
        length
    )
    assert_that(
        all.equal(wgts[, categorical_vars], all_combinations[, categorical_vars])
    )

    wgts_scaled <- wgts[["x"]] / sum(wgts[["x"]])

    design <- apply(design_matrix, 2, function(x) sum(x * wgts_scaled))
    return(design)
}
