library(rbmi)
library(dplyr)

data("antidepressant_data")
dat <- antidepressant_data

## Use expand_locf to add the dropped observations into the dataset
dat <- expand_locf(
    dat,
    PATIENT = levels(dat$PATIENT), # expand by PATIENT and VISIT
    VISIT = levels(dat$VISIT),
    vars = c("BASVAL", "THERAPY"), # fill with LOCF BASVAL and THERAPY
    group = c("PATIENT"),
    order = c("PATIENT", "VISIT")
)

# Create data_ice and set the imputation strategy to JR for
# each patient with at least one missing observation
dat_ice <- dat %>%
    arrange(PATIENT, VISIT) %>%
    filter(is.na(CHANGE)) %>%
    group_by(PATIENT) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(PATIENT, VISIT) %>%
    mutate(strategy = "JR")

# In this dataset, subject 3618 has an intermittent missing values which does not correspond
# to a study drug discontinuation. We therefore remove this subject from `dat_ice`.
# (In the later imputation step, it will automatically be imputed under the default MAR assumption.)
dat_ice <- dat_ice[-which(dat_ice$PATIENT == 3618), ]

# Define the names of key variables in our dataset using `set_vars()`
# Note that the covariates argument can also include interaction terms
vars <- set_vars(
    outcome = "CHANGE",
    visit = "VISIT",
    subjid = "PATIENT",
    group = "THERAPY",
    covariates = c("BASVAL*VISIT", "THERAPY*VISIT")
)

# B*D = 202 to get df >= 100. Here it is 10000
method <- method_bmlmi(
    B = 2500,
    D = 4
)


# Create samples for the imputation parameters
set.seed(987)
drawObj <- draws(
    data = dat,
    data_ice = dat_ice,
    vars = vars,
    method = method
)

# Impute the data
imputeObj <- impute(
    drawObj,
    references = c("DRUG" = "PLACEBO", "PLACEBO" = "PLACEBO")
)

# Fit the analysis model on each imputed dataset
anaObj <- analyse(
    imputeObj,
    ancova,
    vars = set_vars(
        subjid = "PATIENT",
        outcome = "CHANGE",
        visit = "VISIT",
        group = "THERAPY",
        covariates = c("BASVAL")
    )
)

# Pool the results
pool_JR <- pool(
    anaObj,
    conf.level = 0.95,
    alternative = "two.sided"
)


dat_ice$strategy <- "MAR"

# Impute the data
imputeObj <- impute(
    drawObj,
    update_strategy = dat_ice,
    references = c("DRUG" = "PLACEBO", "PLACEBO" = "PLACEBO")
)

# Fit the analysis model on each imputed dataset
anaObj <- analyse(
    imputeObj,
    ancova,
    vars = set_vars(
        subjid = "PATIENT",
        outcome = "CHANGE",
        visit = "VISIT",
        group = "THERAPY",
        covariates = c("BASVAL")
    )
)

# Pool the results
pool_MAR <- pool(
    anaObj,
    conf.level = 0.95,
    alternative = "two.sided"
)

res <- list(
    JR = unlist(pool_JR$pars$trt_7),
    MAR = unlist(pool_MAR$pars$trt_7)
)
res

# saveRDS(res, "res_bmlmi_antidepressant.rds")
