library(glmmTMB)

set.seed(123)

# function for checking whether x is a formula object
is.formula <- function(x) {
    is.call(x) && x[[1]] == quote(`~`)
}

set.seed(101)

n <- 30
nv <- 3

covars <- tibble(
    subjid = 1:n,
    age = rnorm(n),
    group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
    sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F")),
    strata = c(rep("A",n/2), rep("B", n/2))
)

data <- tibble(
    subjid = rep.int(1:n, nv)
) %>%
    left_join(covars, by = "subjid") %>%
    mutate( outcome = rnorm(
        n(),
        age * 3 + (as.numeric(sex) - 1) * 3 + (as.numeric(group) - 1) * 4,
        sd = 3
    )) %>%
    arrange(subjid) %>%
    group_by(subjid) %>%
    mutate( visit = factor(paste0("Visit", 1:n())))  %>%
    ungroup() %>%
    mutate(subjid = as.character(subjid))

vars <- list(
    outcome = "outcome",
    visit = "visit",
    subjid = "subjid",
    group = "group",
    strata = "strata",
    covariates = c("sex", "age", "group*visit"),
    method = "method"
)

formula <- outcome ~ sex + age + visit*group
designmat <- model.matrix(formula, data)

data[sample(1:(nv*n), size = 7), "outcome"] <- NA

names_groups <- c("A", "B")

compute_n_params <- function(cov_struct, nv) {
    if(cov_struct == "us") {
        n_params <- nv*(nv+1)/2
    } else if(cov_struct == "toep") {
        n_params <- 2*nv - 1
    } else if(cov_struct == "cs") {
        n_params <- nv + 1
    } else if(cov_struct == "ar1") {
        n_params <- 2
    }

    return(n_params)
}

test_fit_mmrm <- function(fit, cov_struct, nv, same_cov) {

    n_params <- compute_n_params(cov_struct, nv)

    if(!same_cov) {
        n_params <- 2*n_params
    }

    expect_type(fit, "list")

    expect_vector(fit$beta)
    expect_length(fit$beta, 8)

    expect_type(fit$sigma, "list")
    expect_length(fit$sigma, 2)
    expect_true(is.matrix(fit$sigma[[1]]))
    expect_equal(dim(fit$sigma[[1]]), c(nv,nv))
    expect_true(is.matrix(fit$sigma[[2]]))
    expect_equal(dim(fit$sigma[[2]]), c(nv,nv))

    expect_vector(fit$theta)
    expect_length(fit$theta, n_params)

    expect_true(fit$converged %in% c(TRUE, FALSE))

}

test_that(
    "black spaces and : are correctly removed",
    {
        string_list <- list(
            "subjid" = "subjid  ",
            "visit" = "vis it",
            "group" = "group 12  3",
            "outcome" = " respons e 4"
        )

        string_char <- c("c h  ar 1", "char  2 ")

        string_list_nospaces <- lapply(
            string_list,
            function(x) remove_character(x, " ")
        )

        string_char_nospaces <- remove_character(string_char, " ")

        expect_equal(
            string_list_nospaces,
            list(
                "subjid" = "subjid",
                "visit" = "visit",
                "group" = "group123",
                "outcome" = "response4"
            )
        )

        expect_equal(
            string_char_nospaces,
            c("char1", "char2")
        )

        string_list <- list(
            "subjid" = "subjid:visit",
            "visit" = "visit",
            "group" = "group:visit",
            "outcome" = "response:visit"
        )
        actual_output <- lapply(
            string_list,
            function(x) remove_character(x, ":")
        )

        expect_equal(
            actual_output,
            list(
                "subjid" = "subjidvisit",
                "visit" = "visit",
                "group" = "groupvisit",
                "outcome" = "responsevisit"
            )
        )
    }
)

test_that(
    "designmat_to_formula returns a formula object",
    {
        formula_output <- designmat_to_formula(
            designmat = designmat,
            outcome_var = vars$outcome
        )

        expect_true(is.formula(formula_output))

    })

test_that(
    "random effect expression is built correctly",
    {

        ################## same_cov = TRUE
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

        ################## same_cov = FALSE
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
        ################## same_cov = TRUE
        same_cov = TRUE
        expected_output <- as.formula(outcome ~ sexF + age + visitVisit2 + visitVisit3 + groupB + visitVisit2:groupB +
                                          visitVisit3:groupB + us(0 + visit | subjid))

        formula <- formula_mmrm(
            designmat,
            vars,
            names_groups,
            cov_struct = "us",
            same_cov = same_cov
        )

        expect_true(is.formula(formula))
        expect_equal(formula, expected_output, ignore_attr = TRUE)

        ################## same_cov = FALSE
        same_cov = FALSE
        expected_output <- as.formula(outcome ~ sexF + age + visitVisit2 + visitVisit3 + groupB + visitVisit2:groupB +
                                          visitVisit3:groupB + us(0 + A:visit | subjid) + us(0 + B:visit | subjid))

        formula <- formula_mmrm(
            designmat,
            vars,
            names_groups,
            cov_struct = "us",
            same_cov = same_cov
        )

        expect_true(is.formula(formula))
        expect_equal(formula, expected_output, ignore_attr = TRUE)
    })

test_that(
    "MMRM model fit has expected output structure (same_cov = TRUE)",
    {
        same_cov <- TRUE

        ############# US
        fit <- fit_mmrm(
            designmat = designmat,
            outcome = data$outcome,
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

        expect_length(fit, 4)
        test_fit_mmrm(fit, "us", nv, same_cov)

        ############# TOEP
        fit <- fit_mmrm(
            designmat = designmat,
            outcome = data$outcome,
            subjid = data$subjid,
            visit = data$visit,
            group = data$group,
            vars = vars,
            cov_struct = "toep",
            REML = TRUE,
            same_cov = same_cov,
            initial_values = NULL,
            optimizer = "BFGS"
        )

        expect_length(fit, 4)
        test_fit_mmrm(fit, "toep", nv, same_cov)

        ############# CS
        fit <- fit_mmrm(
            designmat = designmat,
            outcome = data$outcome,
            subjid = data$subjid,
            visit = data$visit,
            group = data$group,
            vars = vars,
            cov_struct = "cs",
            REML = TRUE,
            same_cov = same_cov,
            initial_values = NULL,
            optimizer = "BFGS"
        )

        expect_length(fit, 4)
        test_fit_mmrm(fit, "cs", nv, same_cov)

        ############# AR1
        fit <- fit_mmrm(
            designmat = designmat,
            outcome = data$outcome,
            subjid = data$subjid,
            visit = data$visit,
            group = data$group,
            vars = vars,
            cov_struct = "ar1",
            REML = TRUE,
            same_cov = same_cov,
            initial_values = NULL,
            optimizer = "BFGS"
        )

        expect_length(fit, 4)
        test_fit_mmrm(fit, "ar1", nv, same_cov)

    })

test_that(
    "MMRM model fit has expected output structure (same_cov = FALSE)",
    {
        same_cov <- FALSE

        fit <- fit_mmrm(
            designmat = designmat,
            outcome = data$outcome,
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

        expect_length(fit, 4)
        test_fit_mmrm(fit, "us", nv, same_cov)

    })

test_that(
    "MMRM model fit has expected output structure (REML = FALSE)",
    {
        same_cov <- TRUE

        fit <- fit_mmrm(
            designmat = designmat,
            outcome = data$outcome,
            subjid = data$subjid,
            visit = data$visit,
            group = data$group,
            vars = vars,
            cov_struct = "us",
            REML = FALSE,
            same_cov = same_cov,
            initial_values = NULL,
            optimizer = "L-BFGS-B"
        )

        expect_length(fit, 4)
        test_fit_mmrm(fit, "us", nv, same_cov)

    })

test_that(
    "MMRM returns expected estimates (same_cov = TRUE)",
    {
        same_cov <- TRUE

        fit <- fit_mmrm(
            designmat = designmat,
            outcome = data$outcome,
            subjid = data$subjid,
            visit = data$visit,
            group = data$group,
            vars = vars,
            cov_struct = "us",
            REML = TRUE,
            same_cov = same_cov,
            initial_values = NULL,
            optimizer = "BFGS"
        )

        formula_ext <- outcome ~ sex + age + visit*group + us(0 + visit | subjid)
        control <- glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "BFGS"),
            parallel = 1
        )

        fit_expected <- glmmTMB(
            formula_ext,
            dispformula = ~0,
            data = data,
            REML = TRUE,
            control = control)

        beta <- fixef(fit_expected)$cond
        names(beta) <- remove_character(names(beta), ":")
        sigma <- VarCorr(fit_expected)$cond
        sigma <- lapply(sigma, function(x) as.matrix(data.frame(x)))
        theta <- getME(fit_expected, name = "theta")

        converged <- ifelse(fit_expected$fit$convergence == 0, TRUE, FALSE)

        output_expected <- list(
            beta = beta,
            sigma = list("A" = sigma[[1]], "B" = sigma[[1]]),
            theta = theta,
            converged = converged
        )

        expect_equal(fit, output_expected, ignore_attr = TRUE)

    }
)

test_that(
    "MMRM returns expected estimates (same_cov = FALSE)",
    {
        same_cov <- FALSE

        fit <- fit_mmrm(
            designmat = designmat,
            outcome = data$outcome,
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


        levels(data$group) <- remove_character(levels(data$group), " ")

        # create dummy variables for each arm
        groups_mat <- stats::model.matrix(~ 0 + data$group)
        colnames(groups_mat) <- levels(data$group)
        data <- cbind(data,
                      "A" = groups_mat[,1],
                      "B" = groups_mat[,2])
        formula_ext <- outcome ~ sex + age + visit*group + us(0 + A:visit | subjid) + us(0 + B:visit | subjid)
        control <- glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "L-BFGS-B"),
            parallel = 1
        )

        fit_expected <- glmmTMB(
            formula_ext,
            dispformula = ~0,
            data = data,
            REML = TRUE,
            control = control)

        beta <- fixef(fit_expected)$cond
        names(beta) <- remove_character(names(beta), ":")
        sigma <- VarCorr(fit_expected)$cond
        sigma <- lapply(sigma, function(x) as.matrix(data.frame(x)))
        names(sigma) <- c("A", "B")
        theta <- getME(fit_expected, name = "theta")

        converged <- ifelse(fit_expected$fit$convergence == 0, TRUE, FALSE)

        output_expected <- list(
            beta = beta,
            sigma = sigma,
            theta = theta,
            converged = converged
        )

        expect_equal(fit, output_expected, ignore_attr = TRUE)

    }
)

test_that(
    "MMRM model with multiple optimizers has expected output",
    {
        ########### SINGLE OPTIMIZER
        same_cov <- TRUE

        fit <- fit_mmrm_multiopt(
            designmat = designmat,
            outcome = data$outcome,
            subjid = data$subjid,
            visit = data$visit,
            group = data$group,
            vars = vars,
            cov_struct = "us",
            REML = TRUE,
            same_cov = same_cov,
            initial_values = NULL,
            optimizer = c("BFGS")
        )

        expect_length(fit, 5)
        expect_true(fit$converged)
        expect_equal(fit$optimizer, "BFGS")

        test_fit_mmrm(fit, "us", nv, same_cov)

        ########### TWO OPTIMIZERS
        fit <- fit_mmrm_multiopt(
            designmat = designmat,
            outcome = data$outcome,
            subjid = data$subjid,
            visit = data$visit,
            group = data$group,
            vars = vars,
            cov_struct = "us",
            REML = TRUE,
            same_cov = same_cov,
            initial_values = NULL,
            optimizer = c("Nelder-Mead", "L-BFGS-B")
        )

        expect_length(fit, 5)
        expect_true(fit$converged)
        expect_equal(fit$optimizer, "L-BFGS-B")

        test_fit_mmrm(fit, "us", nv, same_cov)
    })


test_mmrm_formula <- function(data, vars, formula_expr, same_cov) {

    formula <- as.formula(formula_expr)
    designmat <- as_model_df(data, formula)

    fit <- fit_mmrm(
        designmat = designmat[,-1],
        outcome = data$outcome,
        subjid = data$subjid,
        visit = data$visit,
        group = data$group,
        vars = vars,
        cov_struct = "us",
        REML = TRUE,
        same_cov = same_cov,
        initial_values = NULL,
        optimizer = "BFGS"
    )
    names(fit$beta) <- NULL

    if(same_cov) {
        formula_ext <- as.formula(
            paste0(formula_expr, " + us(0 + visit | subjid)")
        )
    } else {
        formula_ext <- as.formula(
            paste0(formula_expr, " + us(0 + A:visit | subjid) + us(0 + B:visit | subjid)")
        )
        names_data <- colnames(data)
        data <- cbind(data, model.matrix(~ 0 + data$group))
        colnames(data) <- c(names_data, c("A", "B"))
    }

    control <- glmmTMBControl(
        optimizer = optim,
        optArgs = list(method = "BFGS"),
        parallel = 1
    )

    fit_expected <- glmmTMB(
        formula_ext,
        dispformula = ~0,
        data = data,
        REML = TRUE,
        control = control)

    beta <- fixef(fit_expected)$cond
    names(beta) <- NULL

    sigma <- VarCorr(fit_expected)$cond
    sigma <- lapply(sigma, function(x) as.matrix(data.frame(x)))
    if(same_cov) {
        sigma = list("A" = sigma[[1]], "B" = sigma[[1]])
    } else {
        sigma = list("A" = sigma[[1]], "B" = sigma[[2]])
    }

    theta <- getME(fit_expected, name = "theta")

    converged <- ifelse(fit_expected$fit$convergence == 0, TRUE, FALSE)

    output_expected <- list(
        beta = beta,
        sigma = sigma,
        theta = theta,
        converged = converged
    )

    expect_equal(fit, output_expected, ignore_attr = TRUE)

}

test_that(
    "MMRM returns expected estimates under different model specifications (same_cov = TRUE)",
    {
        same_cov = TRUE

        formula_expr <- "outcome ~ sex*visit + age*visit + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ sex*group + age*group + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ sex*group*visit + age*group*visit + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ sex + age + sex:age + sex*visit + age:group + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ visit + age*visit*group + sex + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ sex^2"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ age:sex^2 + sex:age*group + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

    }
)

test_that(
    "MMRM returns expected estimates under different model specifications (same_cov = FALSE)",
    {
        same_cov = FALSE

        formula_expr <- "outcome ~ sex*visit + age*visit + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ sex*group + age*group + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ sex*group*visit + age*group*visit + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ sex + age + sex:age + sex*visit + age:group + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ visit + age*visit*group + sex + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ sex^2"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

        formula_expr <- "outcome ~ age:sex^2 + sex:age*group + visit*group"
        test_mmrm_formula(data, vars, formula_expr, same_cov)

    }
)
