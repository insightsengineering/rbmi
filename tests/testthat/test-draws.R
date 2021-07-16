library(dplyr)
library(testthat)

set.seed(101)

n <- 20
nv <- 3

covars <- tibble(
    subjid = 1:n,
    age = rnorm(n),
    group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
    sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F")),
    strata = c(rep("A",n/2), rep("B", n/2))
)

dat <- tibble(
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
    mutate( visit = factor(paste0("Visit ", 1:n())))  %>%
    ungroup() %>%
    mutate(subjid = as.character(subjid))

#dat[c(1,2,3,4,6,7), "outcome"] <- NA


vars <- list(
    outcome = "outcome",
    visit = "visit",
    subjid = "subjid",
    group = "group",
    strata = "strata",
    covariates = c("sex", "age"),
    method = "method"
)

data_ice <- NULL

method <- list(
    covariance = "ar1",
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_imputations = 3
)

test_draws_boot <- function(draws_boot) {
    expect_type(draws_boot, "list")

    expect_true( all(sapply(draws_boot, typeof) == "list") )
    expect_true( all(sapply(draws_boot, length) == 6) )
    expect_true( all(sapply(draws_boot, function(x) length(x$ids_boot)) == n) )

    converged <- sapply(draws_boot, function(x) x$converged)
    structures <- sapply(draws_boot, function(x) x$structure)

    expect_true( all(converged) )
    expect_true( all(structures == "ar1") )

    sigmas <- lapply(draws_boot, function(x) x$sigma)
    expect_true( all(sapply(sigmas, typeof)  == "list") )
    expect_true( all(sapply(sigmas, length)  == 1) )
}

test_that(
    "get bootstrap sample draws properly",
    {

        longdata <- longDataConstructor$new(dat, vars)
        model_df <- as_model_df(data, as_simple_formula(vars))
        scaler <- scalerConstructor$new(model_df)

        draws_boot <- get_bootstrap_samples(
            longdata = longdata,
            method = method,
            scaler = scaler
        )

        expect_length(draws_boot, 2)
        test_draws_boot(draws_boot)
    }
)

test_that(
    "wrapper draws using bootstrap has expected output",
    {

        draws_params <- draws_bootstrap(
            data = dat,
            data_ice = NULL,
            vars = vars,
            method = method
        )
        expect_type(draws_params, "list")
        expect_type(draws_params$samples, "list")

        expect_length(draws_params, 5)
        expect_length(draws_params$samples, 3)
        expect_length(draws_params$structures, 3)
        expect_length(draws_params$optimizers, 3)

        expect_true( all(draws_params$structures == "ar1") )

        test_draws_boot(draws_params$samples)
    })

test_that(
    "draws has expected output",
    {
        boot_draws <- draws(
            dat,
            data_ice = NULL,
            vars,
            method = method_approxbayes(
                covariance = "ar1",
                threshold = 0.01,
                same_cov = TRUE,
                REML = TRUE,
                n_imputations = 3
            )
        )
    }
)



