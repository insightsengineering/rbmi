devtools::load_all()
library(dplyr)
library(testthat)


dat <- structure(
    list(

        subjid = c(
            "Tom", "Tom", "Tom",
            "Harry", "Harry", "Harry",
            "Phil", "Phil", "Phil",
            "Ben", "Ben", "Ben"
        ),

        age = c(
             0.04, 0.04, 0.04,
            -0.14, -0.14, -0.14,
            -0.03, -0.03, -0.03,
            -0.33, -0.33, -0.33
        ),

        group = structure(
            c(2L, 2L, 2L,
              2L, 2L, 2L,
              1L, 1L, 1L,
              1L, 1L, 1L),
            .Label = c("A", "B"),
            class = "factor"
        ),

        sex = structure(
            c(2L, 2L, 2L,
              1L, 1L, 1L,
              1L, 1L, 1L,
              2L, 2L, 2L),
            .Label = c("M", "F"),
            class = "factor"
        ),

        strata = c(
            "A", "A", "A",
            "A", "A", "A",
            "A", "A", "A",
            "B", "B", "B"
        ),

        outcome = c(
            NA, NA, NA,
            NA, 4.14, NA,
            NA, -1.34, 2.41,
            -1.53, 1.03, 2.58
        ),

        visit = structure(
            c(
                1L, 2L, 3L,
                1L, 2L, 3L,
                1L, 2L, 3L,
                1L, 2L, 3L
            ),
            .Label = c("Visit 1", "Visit 2", "Visit 3"),
            class = "factor")
    ),
    row.names = c(NA,  -12L),
    class = c("tbl_df", "tbl", "data.frame")
)


vars <- list(
    outcome = "outcome",
    visit = "visit",
    subjid = "subjid",
    group = "group",
    strata = "strata",
    covariates = c("sex", "age"),
    method = "method"
)



ld <- longDataConstructor$new(
    data = dat,
    vars = vars
)


beta <- c( -2.80, 4.93, 2.01, 4.66, 1.27, 3.14)

sigma <- list(
    "A" = structure(c(1, 0.4, 1.2, 0.4, 4, 3.6, 1.2, 3.6, 9), .Dim = c(3L,3L)),
    "B" = structure(c(1, 0.4, 1.2, 0.4, 4, 3.6, 1.2, 3.6, 9), .Dim = c(3L,3L))
)

obj <- list(
    samples = list(
        list(
            ids =  c("Tom", "Harry", "Phil", "Ben"),
            beta = beta,
            sigma = sigma
        ),
        list(
            ids = c("Ben", "Ben", "Phil"),
            beta = beta,
            sigma = sigma
        )
    ),
    longdata = ld
)




expect_valid_structure <- function(x){
    for( i in x){
        for(j in i){
            cond1 <- all( names(j) %in% c("id", "values"))
            cond2 <- (is.character(j[["id"]]) & length(j[["id"]]) == 1)
            cond3 <- is.numeric( j[["values"]])
            expect_true(cond1 & cond2 & cond3)
        }
    }
}


test_that("Basic Usage",{

    set.seed(101)
    class(obj) <- "bootstrap"
    x1 <- impute( draws = obj, references = c("A" = "A", "B" = "B") )

    class(obj) <- "bayesian"
    x2 <- impute( draws = obj, references = c("A" = "A", "B" = "B") )

    class(obj) <- "condmean"
    x3 <- impute( draws = obj, references = c("A" = "A", "B" = "B") )

    class(obj) <- "condmean"
    x4 <- impute( draws = obj, references = c("A" = "A", "B" = "B") )

    ### The return object is in the expected format
    expect_valid_structure(x1)
    expect_valid_structure(x2)
    expect_valid_structure(x3)

    ### That conditional mean always returns the same deterministic value
    expect_equal(x3, x4)

    ### That the Bayesian / bootstrap return different non-deterministic values
    expect_false(identical(x1,x2))
    expect_false(identical(x2,x3))


    ### That the sample names match those requested
    samp_1_names <- vapply( x1[[1]], function(x) x$id, character(1))
    samp_2_names <- vapply( x1[[2]], function(x) x$id, character(1))
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
        Filter( function(x) x$id == "Tom", x1[[1]])[[1]]$values,
        3
    )

    expect_length(
        Filter( function(x) x$id == "Harry", x1[[1]])[[1]]$values,
        2
    )

    expect_length(
        Filter( function(x) x$id == "Phil", x1[[1]])[[1]]$values,
        1
    )

    expect_length(
        Filter( function(x) x$id == "Ben", x1[[1]])[[1]]$values,
        0
    )
})





















impute <- function(draws,  data_ice, references, strategies = strategies()){}

impute.bootstrap <- function(draws,  data_ice = NULL, references, strategies)

impute.bayesian <- function(draws,  data_ice = NULL, references, strategies){}

impute.condmean <- function(draws,  data_ice = NULL, references, strategies){}

impute_internal <- function(draws, data_ice = NULL, references, strategies, conditionalMean = FALSE){}

transpose_samples <- function(samples){}

untranspose_samples <- function(imputes, indexes){}

invert_indexes <- function(x){}

impute_data_individual <- function(
    id,
    index,
    beta,
    sigma,
    longdata,
    references,
    strategies,
    conditionalMean
){}



impute_outcome <- function(conditional_parameters){}

get_conditional_parameters <- function(pars, values){}

validate_references <- function(references, longdata){}

validate_strategies <- function(strategies, longdata){}

getStrategies <- function(...){}

get_visit_distribution_parameters <- function(dat, beta, sigma){}




