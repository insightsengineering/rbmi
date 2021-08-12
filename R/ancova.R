
#' Analysis of Covariance
#'
#' Performs an analysis of covariance between two groups returning the estimated "treatment effect"
#' as well as the least squared means of each group.
#'
#' @param data A dataframe containing the variables to be used in the model
#' @param vars A named list containing the names of the variables for `group`, `visit`, `outcome` and any `covariates`. 
#' See details.
#' @param visits an optional character vector specifying which visits to perform ancova at. If `NULL` all all available 
#' visits (as determined by `unique(data[[vars$visit]])`) will be looped over.
#'
#' @details
#' TODO
#'
#' @export
ancova <- function(data, vars, visits = NULL) {
    outcome <- vars[["outcome"]]
    group <- vars[["group"]]
    covariates <- vars[["covariates"]]
    visit <- vars[["visit"]]

    expected_vars <- c(extract_covariates(covariates), outcome, group)


    assert_that(
        ! any(visit %in% expected_vars),
        msg = "The `vars$visit` variable cannot be a covariate in an ANCOVA model. Please adjust `vars$covariates` accordingly"
    )

    for (var in c(visit, expected_vars)) {
        assert_that(
            var %in% names(data),
            msg = sprintf("Variable `%s` doesn't exist in data", var)
        )
    }

    assert_that(
        is.character(outcome),
        length(outcome) == 1,
        msg = "`vars$outcome` must be a length 1 character"
    )

    assert_that(
        is.character(group),
        length(group) == 1,
        msg = "`vars$group` must be a length 1 character"
    )

    assert_that(
        is.character(visit),
        length(visit) == 1,
        msg = "`vars$visit` must be a length 1 character"
    )

    assert_that(
        is.character(covariates) | is.null(covariates),
        msg = "`covariates` must be a character vector"
    )

    assert_that(
        is.null(visits) | is.character(visits),
        msg = "`visits` must be NULL or a character vector"
    )

    if (is.null(visits)) {
        visits <- unique(data[[visit]])
    }

    for (i in visits) {
        assert_that(
            i %in% data[[visit]],
            msg = sprintf("Visit `%s` does not appear in `data[[vars$visit]]`", i)
        )
    }

    res <- lapply(
        visits,
        function(x) {
            data2 <- data[data[[visit]] == x, ]
            res <- ancova_single(data2, outcome, group, covariates)
            names(res) <- paste0(names(res), "_", x)
            return(res)
        }
    )
    return(unlist(res, recursive = FALSE))
}



#' Perform
#'
#' @description
#' TODO
#'
#' @param data TODO
#' @param outcome TODO
#' @param group TODO
#' @param covariates TODO
#'
#' @importFrom stats lm coef vcov df.residual
ancova_single <- function(data, outcome, group, covariates) {

    assert_that(
        is.factor(data[[group]]),
        length(levels(data[[group]])) == 2,
        length(unique(data[[group]])) == 2,
        msg = "`data[[vars$group]]` must be a factor variable with 2 levels"
    )

    # Manually convert to dummary variables to make extraction easier
    data[[group]] <- as.numeric(data[[group]]) - 1

    data2 <- data[, c(extract_covariates(covariates), outcome, group)]

    frm <- as_simple_formula(list(group = group, outcome = outcome, covariates = covariates))

    mod <- lm(formula = frm, data = data2)

    args <- list(model = mod)
    args[[group]] <- 0
    lsm0 <- do.call(lsmeans, args)

    args[[group]] <- 1
    lsm1 <- do.call(lsmeans, args)

    x <- list(
        trt = list(
            est = coef(mod)[[group]],
            se = sqrt(vcov(mod)[group, group]),
            df = df.residual(mod)
        ),
        lsm_ref = lsm0,
        lsm_alt = lsm1
    )
    return(x)
}
