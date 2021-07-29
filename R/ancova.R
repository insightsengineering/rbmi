
#' Ancova
#'
#' @description
#' TODO
#'
#' @param data TODO
#' @param outcome TODO
#' @param group TODO
#' @param covariates TODO
#' @param visit TODO
#' @param visit_level TODO
#'
#' @importFrom stats lm coef vcov df.residual
#' 
#' @export
ancova <- function(data, vars, visit_level = NULL){

    outcome <- vars[["outcome"]]
    group <- vars[["group"]]
    covariates <- vars[["covariates"]]
    visit <- vars[["visit"]]


    assert_that(
        is.character(outcome),
        length(outcome) == 1,
        msg = "`outcome` must be a length 1 character"
    )

    assert_that(
        is.character(group),
        length(group) == 1,
        msg = "`outcome` must be a length 1 character"
    )

    assert_that(
        is.character(covariates) | is.null(covariates),
        msg = "`covariates` must be a character vector"
    )

    expected_vars <- c(extract_covariates(covariates), outcome, group)

    if( !is.null(visit_level) | !is.null(visit)){

        assert_that(
            is.character(visit),
            length(visit) == 1,
            msg = "`visit` must be a length 1 character"
        )

        assert_that(
            is.character(visit_level),
            length(visit_level) == 1,
            msg = "`visit_level` must be a length 1 character"
        )

       expected_vars <- c(expected_vars, visit)
    }

    for( var in expected_vars){
        assert_that(
            var %in% names(data),
            msg = sprintf("Variable `%s` doesn't exist in data", var)
        )
    }

    assert_that(
        is.factor(data[[group]]),
        length(levels(data[[group]])) == 2,
        msg = "Group variable `%s` must be a factor variable with 2 levels"
    )

    # Manually convert to dummary variables to make extraction easier
    data[[group]] <- as.numeric(data[[group]]) - 1

    if (!is.null(visit)) {
        data2 <- data[data[[visit]] == visit_level, ]
    } else {
        data2 <- data
    }

    data2 <- data2[, c(extract_covariates(covariates), outcome, group)]

    frm <- as_simple_formula(list(group = group, outcome = outcome, covariates = covariates))

    mod <- lm(formula = frm, data = data2)

    # TODO - LOCF
    x <- list(
        "trt" = list(
            "est" = coef(mod)[[group]],
            "se" = sqrt(vcov(mod)[group, group]),
            "df" = df.residual(mod)
        )
    )
    return(x)
}
