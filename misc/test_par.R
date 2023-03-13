library(dplyr)
#>
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#>
#>     filter, lag
#> The following objects are masked from 'package:base':
#>
#>     intersect, setdiff, setequal, union

data("antidepressant_data")
dat <- antidepressant_data

# Use expand_locf to add rows corresponding to visits with missing outcomes to the dataset
dat <- expand_locf(
    dat,
    PATIENT = levels(dat$PATIENT), # expand by PATIENT and VISIT
    VISIT = levels(dat$VISIT),
    vars = c("BASVAL", "THERAPY"), # fill with LOCF BASVAL and THERAPY
    group = c("PATIENT"),
    order = c("PATIENT", "VISIT")
)

# create data_ice and set the imputation strategy to JR for
# each patient with at least one missing observation
dat_ice <- dat %>%
    arrange(PATIENT, VISIT) %>%
    filter(is.na(CHANGE)) %>%
    group_by(PATIENT) %>%
    slice(1) %>%
    ungroup() %>%
    select(PATIENT, VISIT) %>%
    mutate(strategy = "JR")

# In this dataset, subject 3618 has an intermittent missing values which does not correspond
# to a study drug discontinuation. We therefore remove this subject from `dat_ice`.
# (In the later imputation step, it will automatically be imputed under the default MAR assumption.)
dat_ice <- dat_ice[-which(dat_ice$PATIENT == 3618),]

vars <- set_vars(
    outcome = "CHANGE",
    visit = "VISIT",
    subjid = "PATIENT",
    group = "THERAPY",
    covariates = c("BASVAL*VISIT", "THERAPY*VISIT")
)

method <- method_bayes(
    burn_in = 200,
    burn_between = 5,
    n_samples = 150,
    seed = 675442751
)

set.seed(987)
drawObj <- draws(
    data = dat,
    data_ice = dat_ice,
    vars = vars,
    method = method,
    quiet = TRUE
)

imputeObj <- impute(
    drawObj,
    references = c("DRUG" = "PLACEBO", "PLACEBO" = "PLACEBO")
)

vars = set_vars(
    subjid = "PATIENT",
    outcome = "CHANGE",
    visit = "VISIT",
    group = "THERAPY",
    covariates = c("BASVAL")
)

profvis::profvis({
    analyse(imputeObj, vars = vars)
})

profvis::profvis({
lapply(
    imputeObj$imputations,
    function(x, ...) {
        dat2 <- extract_imputed_df(x, imputeObj$data, NULL)
        ancova(dat2, vars = vars)

    }
)
})

profvis::profvis({
    x <- 1
    y <- rnorm(3000000)
    z <- rnorm(20000000)
})
