
library(dplyr)
library(testthat)

n <- 1000
nv <- 4

covars <- tibble(
    subjid = 1:n,
    age = rnorm(n),
    group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
    sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F")),
    strata = c("A")
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

missing_index <- sample(1:n*nv, size = 100)
dat[missing_index, vars$outcome] <- NA

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

obj <- list(
    samples = replicate(
        n = 100,
        {
            list(
                ids = as.character(1:n),
                beta = c(0.95, -1.76, 2, 3.7, -0.5, 5.2, 2.6),
                sigma = list(
                    "A" = structure(c(1, 0.4, 1.2, 2.4, 0.4, 4, 4.8, 3.2, 1.2, 4.8, 9,
                                      6, 2.4, 3.2, 6, 16), .Dim = c(4L, 4L)),
                    "B" =  structure(c(1, 0.4, 1.2, 2.4, 0.4, 4, 4.8, 3.2, 1.2, 4.8, 9,
                                       6, 2.4, 3.2, 6, 16), .Dim = c(4L, 4L))
                )
            )
        },
        simplify = FALSE
    ),
    longdata = ld
)


time_it({
    x <- impute.bootstrap(
        obj,
        references = c("A" = "B", "B" = "B"),
        strategies = strategies()
    )
})




array()
