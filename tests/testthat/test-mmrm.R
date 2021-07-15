set.seed(123)

is.formula <- function(x) {
    is.call(x) && x[[1]] == quote(`~`)
}

data <- data.frame(
    pred = rnorm(42),
    subjid = as.factor(rep(1:14, each = 3)),
    visit = as.factor(rep(c(1,2,3), 14)),
    group = as.factor(rep(c("A", "B"), each = 21))
)
data$response <- data$pred + 0.5*(as.numeric(data$group) - 1) + rnorm(42, sd = 0.1)

vars <- list(
    "subjid" = "subjid",
    "visit" = "visit",
    "group" = "group",
    "response" = "response"
)

formula <- response ~ pred + visit*group
designmat <- model.matrix(formula, data)

names_groups <- c("A", "B")

test_that(
    "designmat_to_formula returns a formula object",
    {
        formula_output <- designmat_to_formula(
            designmat = designmat,
            outcome_var = vars$response
        )

        expect_true(is.formula(formula_output))

    })

test_that(
    "random effect expression is built correctly",
    {
        same_cov = TRUE
        expected_output <- " + us(0 + visit | subjid)"

        expect_equal(random_effects_expr(
            vars,
            names_groups,
            cov_struct = "us",
            same_cov = same_cov
        ),
        expected_output)

        expect_error(
            random_effects_expr(
                vars,
                names_groups,
                cov_struct = "unstructured",
                same_cov = same_cov
            )
        )

        same_cov = FALSE
        expected_output <- " + us(0 + A:visit | subjid) + us(0 + B:visit | subjid)"

        expect_equal(
            random_effects_expr(
                vars,
                names_groups,
                cov_struct = "us",
                same_cov = same_cov
            ),
            expected_output
        )

        expected_output <- " + toep(0 + A:visit | subjid) + toep(0 + B:visit | subjid)"
        expect_equal(
            random_effects_expr(
                vars,
                names_groups,
                cov_struct = "toep",
                same_cov = same_cov
            ),
            expected_output
        )

    })

test_that(
    "formula is build correctly",
    {
        same_cov = TRUE
        expected_output <- as.formula(response ~ pred + visit2 + visit3 + groupB + visit2:groupB +
                                          visit3:groupB + us(0 + visit | subjid))

        formula <- formula_mmrm(
            designmat,
            vars,
            names_groups,
            cov_struct = "us",
            same_cov = same_cov
        )

        # set formula environment to be the same since we don't want to test it
        attr(formula, ".Environment") <- attr(expected_output, ".Environment")

        expect_true(is.formula(formula))
        expect_equal(formula, expected_output)

        same_cov = FALSE
        expected_output <- as.formula(response ~ pred + visit2 + visit3 + groupB + visit2:groupB +
                                          visit3:groupB + us(0 + A:visit | subjid) + us(0 + B:visit | subjid))

        formula <- formula_mmrm(
            designmat,
            vars,
            names_groups,
            cov_struct = "us",
            same_cov = same_cov
        )

        # set formula environment to be the same since we don't want to test it
        attr(formula, ".Environment") <- attr(expected_output, ".Environment")

        expect_true(is.formula(formula))
        expect_equal(formula, expected_output)
    })

test_that(
    "MMRM model fit has expected output structure (same_cov = TRUE)",
    {
        same_cov <- TRUE

        fit <- fit_mmrm(
        designmat = designmat,
        outcome = data$response,
        subjid = data$subjid,
        visit = data$visit,
        group = data$group,
        vars = vars,
        cov_struct = "us",
        REML = TRUE,
        same_cov = same_cov,
        initial_values = NULL,
        optimizer = "L-BFGS-B"
        )

        expect_type(fit, "list")
        expect_length(fit, 5)

        expect_vector(fit$beta)
        expect_length(fit$beta, 7)

        expect_type(fit$sigma, "list")
        expect_length(fit$sigma, 1)
        expect_true(is.matrix(fit$sigma[[1]]))
        expect_equal(dim(fit$sigma[[1]]), c(3,3))

        expect_vector(fit$theta)
        expect_length(fit$theta, 6)

        expect_true(fit$converged %in% c(TRUE, FALSE))

        expect_equal(fit$structure, "us")

    })

test_that(
    "MMRM model fit has expected output structure (same_cov = FALSE)",
    {
        same_cov <- FALSE

        fit <- fit_mmrm(
            designmat = designmat,
            outcome = data$response,
            subjid = data$subjid,
            visit = data$visit,
            group = data$group,
            vars = vars,
            cov_struct = "us",
            REML = TRUE,
            same_cov = same_cov,
            initial_values = NULL,
            optimizer = "L-BFGS-B"
        )

        expect_type(fit, "list")
        expect_length(fit, 5)

        expect_vector(fit$beta)
        expect_length(fit$beta, 7)

        expect_type(fit$sigma, "list")
        expect_length(fit$sigma, 2)
        expect_true(is.matrix(fit$sigma[[1]]))
        expect_equal(dim(fit$sigma[[1]]), c(3,3))
        expect_true(is.matrix(fit$sigma[[2]]))
        expect_equal(dim(fit$sigma[[2]]), c(3,3))

        expect_vector(fit$theta)
        expect_length(fit$theta, 12)

        expect_true(fit$converged %in% c(TRUE, FALSE))

        expect_equal(fit$structure, "us")

    })
