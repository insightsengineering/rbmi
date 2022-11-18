########################
#
# Create objects that are used in example
#
#
#

library(dplyr)
devtools::load_all()

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

# Define the names of key variables in our dataset and
# the covariates included in the imputation model using `set_vars()`
# Note that the covariates argument can also include interaction terms
vars <- set_vars(
    outcome = "CHANGE",
    visit = "VISIT",
    subjid = "PATIENT",
    group = "THERAPY",
    covariates = c("BASVAL*VISIT", "THERAPY*VISIT")
)

# Define which imputation method to use (here: Bayesian multiple imputation with 150 imputed datsets)
method <- method_bayes(
    burn_in = 200,
    burn_between = 5,
    n_samples = 150
)

# Create samples for the imputation parameters by running the draws() function
set.seed(987)
drawobj <- draws(
    data = dat,
    data_ice = dat_ice,
    vars = vars,
    method = method,
    quiet = TRUE
)

imputeobj <- impute(
    drawobj,
    references = c("DRUG" = "PLACEBO", "PLACEBO" = "PLACEBO")
)

delta_df <- delta_template(imputeobj) %>%
    as_tibble() %>%
    mutate(delta = if_else(THERAPY == "DRUG" & is_missing , 5, 0)) %>%
    select(PATIENT, VISIT, delta)

saveRDS(drawobj, 'inst/extdata/drawobj.rds')
saveRDS(imputeobj, 'inst/extdata/imputeobj.rds')
saveRDS(delta_df, 'inst/extdata/delta_df.rds')

