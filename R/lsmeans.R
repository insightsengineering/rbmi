


#' Title - TODO
#'
#' @param data TODO
#' @param mterms TODO
#' @param fix TODO
#'
lscombinations <- function(data, mterms, fix) {
    x <- lapply(
        mterms,
        function(v) {
            values <- data[[v]]
            if (is.factor(values)) {
                val <- factor(
                    ife(v %in% names(fix), fix[[v]], unique(values)),
                    levels = levels(values)
                )
            } else if (is.numeric(values)) {
                val <- ife(
                    v %in% names(fix),
                    fix[[v]],
                    mean(values, na.rm = TRUE)
                )
            } else {
                stop("invalid data type")
            }
            return(val)
        }
    )
    names(x) <- mterms
    return(x)
}


#' Least Square Means
#'
#' Estimates the least square means from a linear model. This is
#' essentially where we generate a prediction from the model
#' by fitting it to some hypothetical observation that is constructed
#' by averaging the data. See details for more information.
#'
#' @details
#' Numeric variables are evaluated at the mean across the entire dataset
#' (after removing missing values)
#' Factor variables are evaluated at all levels (including combinations
#' with other factor variables) with the final return value being
#' average across all the predictions generated at each of these levels.
#'
#' Use the `...` argument to fix specific variables to specific values.
#'
#' See the references for identical implementations as done in SAS and via
#' the emmeans package. This function attempts to re-implement the
#' emmeans derivation for standard lm's but without having to include
#' all of their dependencies
#'
#' @param model A model created by lm
#' @param ... Fixes specific variables to specific values i.e.
#' `trt = 1` or `age = 50`. The name of the argument must be the name
#' of the variable within the dataset
#'
#' @references \url{https://cran.r-project.org/web/packages/emmeans/index.html}
#' @references \url{https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.3/statug/statug_glm_details41.htm}
#' @examples
#' \dontrun{
#' mod <- lm( Sepal.Length ~ Species + Petal.Length, data = iris)
#' lsmeans(mod)
#' lsmeans(mod, Species = "virginica")
#' lsmeans(mod, Species = "versicolor")
#' lsmeans(mod, Species = "versicolor", Petal.Length = 1)
#' }
#' @importFrom stats model.matrix terms reformulate
lsmeans <- function(model, ...) {

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

    var_list <- lscombinations(data, covars, fix)

    design <- matrix(
        apply(model.matrix(frm, expand.grid(var_list)), 2, mean),
        ncol = 1
    )

    beta <- matrix(coef(model), nrow = 1)

    list(
        est = as.vector(beta %*% design),
        se = as.vector(sqrt(t(design) %*% vcov(model) %*% design)),
        df = df.residual(model)
    )
}

