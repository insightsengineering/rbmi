
#' Ancova
#'
#' @description
#' TODO
#'
#' @param data TODO
#' @param vars TODO
#' @param visit_level TODO
#'
#' @importFrom stats lm coef vcov df.residual
#'
#' @export
ancova <- function(data, vars, visit_level = NULL) {
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

    if(!is_absent(visit)) {
        assert_that(
            ! visit %in% expected_vars,
            msg = "The `vars$visit` variable cannot be a covariate in an ANCOVA model. Please adjust `vars$covariates` accordingly"
        )
    }

    if (!is_absent(visit_level)) {

        assert_that(
            is.character(visit),
            length(visit) == 1,
            nchar(visit) >= 1,
            msg = "`vars$visit` must be a single non-empty string"
        )

        assert_that(
            is.character(visit_level),
            length(visit_level) == 1,
            nchar(visit_level) >= 1,
            msg = "`visit_level` must be a single non-empty string"
        )

       expected_vars <- c(expected_vars, visit)
    }

    for (var in expected_vars) {
        assert_that(
            var %in% names(data),
            msg = sprintf("Variable `%s` doesn't exist in data", var)
        )
    }

    if (!is_absent(visit_level)) {
        assert_that(
            visit_level %in% data[[visit]],
            msg = sprintf("`%s` is not present within `data[[vars$visit]]`", visit_level)
        )
        data <- data[data[[visit]] == visit_level, ]
    }

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
        lsm_0 = lsm0,
        lsm_1 = lsm1
    )
    return(x)
}
