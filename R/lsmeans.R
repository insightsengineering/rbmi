


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


#' Title - TODO
#' 
#' @param model TODO
#' @param ... TODO
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
