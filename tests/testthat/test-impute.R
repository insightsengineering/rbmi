suppressPackageStartupMessages({
    library(dplyr)
    library(testthat)
    library(tibble)
})





test_that("Basic Usage", {

    dat <- tibble(
        subjid = factor(rep(c("Tom", "Harry", "Phil", "Ben"), each = 3), levels = c("Tom", "Harry", "Phil", "Ben")),
        age = rep(c(0.04, -0.14, -0.03, -0.33), each = 3),
        group = factor(rep(c("B", "B", "A", "A"), each = 3), levels = c("A", "B")),
        sex = factor(rep(c("F", "M", "M", "F"), each = 3), levels = c("M", "F")),
        strata = rep(c("A", "A", "A", "B"), each = 3),
        visit = factor(rep(c("Visit 1", "Visit 2", "Visit 3"), 4)),
        outcome = c(
            NA, NA, NA,
            NA, 4.14, NA,
            NA, -1.34, 2.41,
            -1.53, 1.03, 2.58
        )
    )

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "subjid",
        group = "group",
        strata = "strata",
        covariates = c("sex", "age"),
        strategy = "strategy"
    )

    ld <- longDataConstructor$new(
        data = dat,
        vars = vars
    )

    beta <- c(-2.80, 4.93, 2.01, 4.66, 1.27, 3.14)

    sigma <- list(
        "A" = structure(c(1, 0.4, 1.2, 0.4, 4, 3.6, 1.2, 3.6, 9), .Dim = c(3L, 3L)),
        "B" = structure(c(1, 0.4, 1.2, 0.4, 4, 3.6, 1.2, 3.6, 9), .Dim = c(3L, 3L))
    )

    draws_args <- list(
        samples = as_sample_list(
            as_sample_single(
                ids = c("Tom", "Harry", "Phil", "Ben"),
                beta = beta,
                sigma = sigma
            ),
            as_sample_single(
                ids = c("Ben", "Ben", "Phil"),
                beta = beta,
                sigma = sigma
            )
        ),
        data = ld,
        formula = x ~ y
    )

    draws_args$method <- method_approxbayes(n_samples = 2)
    drawsObj1 <- do.call(as_draws, draws_args)

    draws_args$method <- method_condmean(n_samples = 1)
    drawsObj2 <- do.call(as_draws, draws_args)

    set.seed(101)
    x1 <- impute(draws = drawsObj1, references = c("A" = "A", "B" = "B"))
    x2 <- impute(draws = drawsObj1, references = c("A" = "A", "B" = "B"))

    x3 <- impute(draws = drawsObj2, references = c("A" = "A", "B" = "B"))
    x4 <- impute(draws = drawsObj2, references = c("A" = "A", "B" = "B"))

    expect_valid_structure <- function(x) {
        for (i in x$imputations) {
            for (j in i) {
                cond1 <- all(names(j) %in% c("id", "values"))
                cond2 <- is.character(j[["id"]]) & length(j[["id"]]) == 1
                cond3 <- is.numeric(j[["values"]])
                expect_true(cond1 & cond2 & cond3)
            }
        }
    }

    ### The return object is in the expected format
    expect_valid_structure(x1)
    expect_valid_structure(x2)
    expect_valid_structure(x3)
    expect_valid_structure(x4)

    ### That conditional mean always returns the same deterministic value
    expect_equal(x3, x4)

    ### That the Bayesian / bootstrap return different non-deterministic values
    expect_false(identical(x1, x2))
    expect_false(identical(x2, x3))


    ### That the sample names match those requested
    samp_1_names <- vapply(x1$imputations[[1]], function(x) x$id, character(1))
    samp_2_names <- vapply(x1$imputations[[2]], function(x) x$id, character(1))
    real_1_names <- c("Tom", "Harry", "Phil", "Ben")
    real_2_names <- c("Ben", "Ben", "Phil")

    expect_equal(
        samp_1_names[order(samp_1_names)],
        real_1_names[order(real_1_names)]
    )

    expect_equal(
        samp_2_names[order(samp_2_names)],
        real_2_names[order(real_2_names)]
    )

    ### That specific names have the correct number of missing values
    expect_length(
        Filter(function(x) x$id == "Tom", x1$imputations[[1]])[[1]]$values,
        3
    )

    expect_length(
        Filter(function(x) x$id == "Harry", x1$imputations[[1]])[[1]]$values,
        2
    )

    expect_length(
        Filter(function(x) x$id == "Phil", x1$imputations[[1]])[[1]]$values,
        1
    )

    expect_length(
        Filter(function(x) x$id == "Ben", x1$imputations[[1]])[[1]]$values,
        0
    )
})







test_that("transpose_samples", {

    input <- list(
        list(
            ids =  c("Tom", "Harry", "Phil", "Ben"),
            beta = c(1, 2, 3),
            sigma = list("A" = 9, "B" = 8, "C" = 7)
        ),
        list(
            ids = c("Adam", "Ben", "Ben", "Phil"),
            beta = c(4, 5, 6),
            sigma = list("A" = iris, "B" = 2, "C" = 3)
        )
    )

    output_actual <- transpose_samples(input)

    output_expected <- list(
        beta = list(
            c(1, 2, 3),
            c(4, 5, 6)
        ),
        sigma = list(
            "A" = list(9, iris),
            "B" = list(8, 2),
            "C" = list(7, 3)
        ),
        index = list(
            "Tom" = c(1),
            "Harry" = c(1),
            "Phil" = c(1, 2),
            "Ben" = c(1, 2, 2),
            "Adam" = c(2)
        )
    )

    expect_equal(output_actual, output_expected)
})






test_that("invert_indexes", {

    input <- list(
        c("Tom", "Harry", "Phil", "Ben", "Harry"),
        c("Adam", "Ben", "Ben", "Phil")
    )

    output_actual <- invert_indexes(input)

    output_expected <- list(
        "Tom" = c(1),
        "Harry" = c(1, 1),
        "Phil" = c(1, 2),
        "Ben" = c(1, 2, 2),
        "Adam" = c(2)
    )

    expect_equal(output_actual, output_expected)

})






test_that("untranspose_imputations", {

    input_imputes <- list(
        list(id = "Ben", values = list(numeric(0), numeric(0), numeric(0))),
        list(id = "Harry", values = list(c(1, 2))),
        list(id = "Phil", values = list(c(3, 4), c(5, 6))),
        list(id = "Tom", values = list(c(7, 8, 9)))
    )

    sample_ids <- list(
        c("Ben", "Harry", "Phil", "Tom"),
        c("Ben", "Ben", "Phil")
    )

    output_actual <- untranspose_imputations(input_imputes, sample_ids)

    output_expected <- list(
        as_imputation_list(
            as_imputation_single(id = "Ben", values = numeric(0)),
            as_imputation_single(id = "Harry", values = c(1, 2)),
            as_imputation_single(id = "Phil", values = c(3, 4)),
            as_imputation_single(id = "Tom", values = c(7, 8, 9))
        ),
        as_imputation_list(
            as_imputation_single(id = "Ben", values = numeric(0)),
            as_imputation_single(id = "Ben", values = numeric(0)),
            as_imputation_single(id = "Phil", values = c(5, 6))
        )
    )
    expect_equal(output_actual, output_expected)


    sample_ids <- list(
        c("Ben"),
        c("Ben", "Ben", "Phil"),
        c("Phil", "Tom"),
        c("Harry")
    )
    output_actual <- untranspose_imputations(input_imputes, sample_ids)
    output_expected <- list(
        as_imputation_list(
            as_imputation_single(id = "Ben", values = numeric(0))
        ),
        as_imputation_list(
            as_imputation_single(id = "Ben", values = numeric(0)),
            as_imputation_single(id = "Ben", values = numeric(0)),
            as_imputation_single(id = "Phil", values = c(3, 4))
        ),
        as_imputation_list(
            as_imputation_single(id = "Phil", values = c(5, 6)),
            as_imputation_single(id = "Tom", values = c(7, 8, 9))
        ),
        as_imputation_list(
            as_imputation_single(id = "Harry", values = c(1, 2))
        )
    )
    expect_equal(output_actual, output_expected)



    sample_ids <- list(
        c("Ben"),
        c("Ben", "Ben", "Phil", "Phil"),
        c("Phil", "Tom"),
        c("Harry")
    )
    expect_error(
        untranspose_imputations(input_imputes, sample_ids),
        "subscript out of bounds"
    )


    sample_ids <- list(
        c("James"),
        c("Ben", "Ben", "Phil"),
        c("Phil", "Tom"),
        c("Harry")
    )
    expect_error(
        untranspose_imputations(input_imputes, sample_ids),
        "sample_ids contains an id not available in imputations"
    )


    sample_ids <- list(
        c("Ben", "Phil"),
        c("Phil", "Tom"),
        c("Harry")
    )
    expect_error(
        untranspose_imputations(input_imputes, sample_ids),
        "Not all imputations have been used"
    )

})





test_that("get_conditional_parameters", {

    input_pars <- list(
        "mu" = c(2, 3, 4),
        "sigma" = structure(c(1, 0.4, 1.2, 0.4, 4, 3.6, 1.2, 3.6, 9), .Dim = c(3L, 3L))
    )

    input_values <- c(NA, NA, 8)
    output_actual <- get_conditional_parameters(input_pars, input_values)
    output_expected <- list(
        mu = structure(c(2.53333333333333, 4.6), .Dim = c(2L, 1L)),
        sigma = structure(c(0.84,  -0.0799999999999999, -0.08, 2.56), .Dim = c(2L, 2L))
    )
    expect_equal(output_actual, output_expected)


    input_values <- c(NA, 8, NA)
    output_actual <- get_conditional_parameters(input_pars, input_values)
    output_expected <- list(
        mu = structure(c(2.5, 8.5), .Dim = c(2L, 1L)),
        sigma = structure(c(0.96, 0.84, 0.84, 5.76), .Dim = c(2L, 2L))
    )
    expect_equal(output_actual, output_expected)


    input_values <- c(1, NA, 2)
    output_actual <- get_conditional_parameters(input_pars, input_values)
    output_expected <- list(
        mu = structure(c(2.26984126984127), .Dim = c(1L, 1L)),
        sigma = structure(c(2.55238095238095), .Dim = c(1L, 1L))
    )
    expect_equal(output_actual, output_expected)


    ### If all values are unknown then it should just return the full
    ### distribution as there is nothing to condition over
    input_values <- c(NA, NA, NA)
    output_actual <- get_conditional_parameters(input_pars, input_values)
    output_expected <- input_pars
    expect_equal(output_actual, output_expected)


    ### If all values are already available then return nothing as
    ### nothing needs to be imputed
    input_values <- c(1, 2, 3)
    output_actual <- get_conditional_parameters(input_pars, input_values)
    output_expected <- list(mu = numeric(0), sigma = numeric(0))
    expect_equal(output_actual, output_expected)
})





test_that("impute_outcome", {


    ##### Univariate
    x <- impute_outcome(list(mu = 5, sigma = 10))
    expect_length(x, 1)
    expect_true(is.numeric(x))

    set.seed(101)
    x <- replicate(n = 20000, impute_outcome(list(mu = 5, sigma = 10)))
    mu <- mean(x)
    sig <- sd(x)^2
    expect_true(4.9 <= mu  & mu <= 5.1)
    expect_true(9.8 <= sig & sig <= 10.2)



    ##### Multivariate

    # as_covmat( c(2, 4, 6), c(0.3, 0.5, 0.7)) %>% dput
    pars <- list(
        mu = c(8, 10, 12),
        sigma = structure(c(4, 2.4, 6, 2.4, 16, 16.8, 6, 16.8, 36), .Dim = c(3L, 3L))
    )

    x <- impute_outcome(pars)
    expect_length(x, 3)
    expect_true(is.numeric(x))

    set.seed(101)
    vals <- replicate(n = 20000, impute_outcome(pars), simplify = FALSE)
    x <- matrix(unlist(vals), ncol = 3, byrow = TRUE)

    # Means
    mu <- apply(x, 2, mean)
    expect_true(all((pars$mu - 0.1) <= mu  & mu <= (pars$mu + 0.1)))

    # Variances
    sig <- apply(x, 2, sd)
    d_sig <- sqrt(diag(pars$sigma))
    expect_true(all((d_sig - 0.1) <= sig & sig <= (d_sig + 0.1)))

    # Correlations
    corr <- cor(x)
    corr_expec <- c(0.3, 0.5, 0.7)
    corr_obs <- c(corr[1, 2], corr[1, 3], corr[2, 3])
    expect_true(all((corr_obs - 0.1) <= corr_expec & corr_expec <= (corr_obs + 0.1)))


    ###### Non-compatible matrices
    pars <- list(
        mu = c(8, 10),
        sigma = structure(c(4, 2.4, 6, 2.4, 16, 16.8, 6, 16.8, 36), .Dim = c(3L, 3L))
    )
    expect_error(impute_outcome(pars))


    pars <- list(
        mu = c(8),
        sigma = structure(c(4, 2.4, 6, 2.4, 16, 16.8, 6, 16.8, 36), .Dim = c(3L, 3L))
    )
    expect_error(impute_outcome(pars))


    pars <- list(
        mu = c(8, 2),
        sigma = 1
    )
    expect_error(impute_outcome(pars))


    ##### Missing value handling
    pars <- list(
        mu = c(2, NA),
        sigma = 1
    )
    expect_error(impute_outcome(pars))


    pars <- list(
        mu = c(1, 2, 4),
        sigma = structure(c(4, 2.4, 6, 2.4, NA, 16.8, 6, 16.8, 36), .Dim = c(3L, 3L))
    )
    expect_error(impute_outcome(pars))


    pars <- list(
        mu = c(NA),
        sigma = 1
    )
    expect_error(impute_outcome(pars))


    pars <- list(
        mu = c(1),
        sigma = NA
    )
    expect_error(impute_outcome(pars))
})





test_that("get_visit_distribution_parameters", {

    beta <- list(c(1, 2, 3), c(4, 5, 6))
    dat <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
    sigma <- list(1, 5)

    x <- get_visit_distribution_parameters(dat, beta, sigma)
    expect_equal(
        x[[1]]$mu,
        c(1 * 1 + 2 * 3 + 3 * 5,  1 * 2 + 2 * 4 + 3 * 6)
    )
    expect_equal(
        x[[2]]$mu,
        c(4 * 1 + 5 * 3 + 6 * 5,  4 * 2 + 5 * 4 + 6 * 6)
    )
    expect_equal(x[[1]]$sigma, 1)
    expect_equal(x[[2]]$sigma, 5)

    beta <- list(c(1, 2, 3), c(4, 5, 6))
    dat <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
    sigma <- list(1)
    expect_error(get_visit_distribution_parameters(dat, beta, sigma))

    beta <- list(c(1, 2), c(4, 5))
    dat <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
    sigma <- list(1, 5)
    expect_error(get_visit_distribution_parameters(dat, beta, sigma))

    beta <- list(c(1, 2, 3), c(4, 5, 6))
    dat <- data.frame(a = c(1, 2), b = c(3, 4))
    sigma <- list(1, 5)
    expect_error(get_visit_distribution_parameters(dat, beta, sigma))

    beta <- list(c(1, 2, 3), c(4, 5))
    dat <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
    sigma <- list(1, 5)
    expect_error(get_visit_distribution_parameters(dat, beta, sigma))
})




test_that("validate_strategies", {

    dat <- tibble(
        subjid = factor(rep(c("Tom", "Harry", "Phil", "Ben"), each = 3), levels = c("Tom", "Harry", "Phil", "Ben")),
        age = rep(c(0.04, -0.14, -0.03, -0.33), each = 3),
        group = factor(rep(c("B", "B", "A", "A"), each = 3), levels = c("A", "B")),
        sex = factor(rep(c("F", "M", "M", "F"), each = 3), levels = c("M", "F")),
        strata = rep(c("A", "A", "A", "B"), each = 3),
        visit = factor(rep(c("Visit 1", "Visit 2", "Visit 3"), 4)),
        outcome = c(
            NA, NA, NA,
            NA, 4.14, NA,
            NA, -1.34, 2.41,
            -1.53, 1.03, 2.58
        )
    )

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "subjid",
        group = "group",
        strata = "strata",
        covariates = c("sex", "age"),
        strategy = "strategy"
    )

    ld <- longDataConstructor$new(
        data = dat,
        vars = vars
    )

    strats <- list("MAR" = function(x) x)
    expect_true(validate_strategies(strats, ld$strategies))
    expect_true(validate_strategies(strats, "MAR"))
    expect_error(validate_strategies(strats, "NMAR"))

    strats <- list("MAR" = function(x) x, "NMAR" = function(x) x)
    expect_true(validate_strategies(strats, "NMAR"))

    strats <- list("MAR" = function(x) x, "NMAR" = 1)
    expect_error(validate_strategies(strats, "NMAR"))

    strats <- c("NMAR")
    expect_error(validate_strategies(strats, "NMAR"))

})





test_that("validate_references", {

    control <- factor(c("A", "B", "C"), levels = c("A", "B", "C", "D"))

    ref <- c("A" = "B") %>% as_class("references")
    expect_true(validate(ref, control))

    ref <- c("A" = "B", "C" = "A")  %>% as_class("references")
    expect_true(validate(ref, control))

    ref <- c("A" = "B", "B" = "B", "C" = "C")  %>% as_class("references")
    expect_true(validate(ref, control))

    ref <- c("X" = "A") %>% as_class("references")
    expect_error(validate(ref, control))

    ref <- c("A" = "X") %>% as_class("references")
    expect_error(validate(ref, control))

    ref <- c("A") %>% as_class("references")
    expect_error(validate(ref, control))

    ref <- c(1, 2, 3) %>% as_class("references")
    expect_error(validate(ref, control))

    ref <- factor("A") %>% as_class("references")
    expect_error(validate(ref, control))

    ref <- c("A" = NA,  "B" = "C") %>% as_class("references")
    expect_error(validate(ref, control))

    ref <- c("A", "B" = "C") %>% as_class("references")
    expect_error(validate(ref, control))

    ref <- list() %>% as_class("references")
    expect_error(validate(ref, control))
})








test_that("impute can recover known values", {

    vars <- set_vars(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group",
        strategy = "strategy",
        covariates = c("cov1", "cov1*group")
    )

    dat <- tibble(
        visit = factor(rep(c("v1", "v2", "v3"), 4), levels = c("v1", "v2", "v3")),
        id = factor(rep(c("1", "2", "3", "4"), each = 3)),
        group = factor(rep(c("A", "B"), each = 6)),
        cov1 =    c(1, 1, 1,      2, 2, 2,     3, 3, 3,    4, 4, 4),
        outcome = c(1, NA, NA,    4, 3, 6,     1, 1, 1,    5, NA, 6)
    )


    ld <- longDataConstructor$new(dat, vars)

    #           1     2     3  4      5        6
    # outcome ~ 1 + group + visit + cov1 + cov1*group
    dobj <- as_draws(
        samples = as_sample_list(
            as_sample_single(
                ids = c("1", "2", "4"),
                beta = c(1, 2, 3, 4, 5, 6),
                sigma = list(
                    "A" = diag(c(1, 1, 1)),
                    "B" = diag(c(1, 1, 1))
                )
            )
        ),
        data = ld,
        method = method_condmean(n_samples = 4),
        formula = x ~ y
    )

    x <- impute(dobj, c("A" = "B", "B" = "B"))

    expect_length(x$imputations, 1)
    expect_length(x$imputations[[1]], 3)
    expect_equal(x$imputations[[1]][[1]]$values, c(9, 10))
    expect_equal(x$imputations[[1]][[2]]$values, numeric(0))
    expect_equal(x$imputations[[1]][[3]]$values, c(50))
    expect_equal(vapply(x$imputations[[1]], function(x) x$id, character(1)), c("1", "2", "4"))


    dobj$samples[[1]]$ids <- c("4", "4", "1", "3")
    x <- impute(dobj, c("A" = "B", "B" = "B"))

    expect_length(x$imputations, 1)
    expect_length(x$imputations[[1]], 4)
    expect_equal(x$imputations[[1]][[1]]$values, 50)
    expect_equal(x$imputations[[1]][[2]]$values, 50)
    expect_equal(x$imputations[[1]][[3]]$values, c(9, 10))
    expect_equal(x$imputations[[1]][[4]]$values, numeric(0))
    expect_equal(vapply(x$imputations[[1]], function(x) x$id, character(1)), c("4", "4", "1", "3"))




    dobj$samples[[2]] <- as_sample_single(
        ids = c("2", "1", "4"),
        beta = c(6, 5, 4, 3, 2, 1),
        sigma = list(
            "A" = diag(c(1, 1, 1)),
            "B" = diag(c(1, 1, 1))
        )
    )
    x <- impute(dobj, c("A" = "B", "B" = "B"))
    expect_length(x$imputations, 2)

    expect_length(x$imputations[[1]], 4)
    expect_equal(x$imputations[[1]][[1]]$values, 50)
    expect_equal(x$imputations[[1]][[2]]$values, 50)
    expect_equal(x$imputations[[1]][[3]]$values, c(9, 10))
    expect_equal(x$imputations[[1]][[4]]$values, numeric(0))
    expect_equal(vapply(x$imputations[[1]], function(x) x$id, character(1)), c("4", "4", "1", "3"))

    expect_length(x$imputations[[2]], 3)
    expect_equal(x$imputations[[2]][[1]]$values, numeric(0))
    expect_equal(x$imputations[[2]][[2]]$values, c(12, 11))
    expect_equal(x$imputations[[2]][[3]]$values, c(27))
    expect_equal(vapply(x$imputations[[2]], function(x) x$id, character(1)), c("2", "1", "4"))




    dat_ice <- tibble(
        visit = "v3",
        strategy = "JR",
        id = "1"
    )

    ld <- longDataConstructor$new(dat, vars)
    ld$set_strategies(dat_ice)

    dobj <- as_draws(
        samples = as_sample_list(
            as_sample_single(
                ids = c("1", "2", "4"),
                beta = c(1, 2, 3, 4, 5, 6),
                sigma = list(
                    "A" = diag(c(1, 1, 1)),
                    "B" = diag(c(1, 1, 1))
                )
            )
        ),
        data = ld,
        method = method_condmean(n_samples = 1),
        formula = x~y
    )

    x <- impute(dobj, c("A" = "B", "B" = "B"))

    expect_length(x$imputations, 1)
    expect_length(x$imputations[[1]], 3)
    expect_equal(x$imputations[[1]][[1]]$values, c(9, 18))
    expect_equal(x$imputations[[1]][[2]]$values, numeric(0))
    expect_equal(x$imputations[[1]][[3]]$values, c(50))
    expect_equal(vapply(x$imputations[[1]], function(x) x$id, character(1)), c("1", "2", "4"))
})
