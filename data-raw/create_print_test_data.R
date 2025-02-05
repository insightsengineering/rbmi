########################
#
# Generate standardised data for use in testing print methods
#
#
#

library(dplyr)
library(tidyr)
devtools::load_all()


.test_print <- new.env(parent = emptyenv())


get_data <- function(n) {
    sigma <- as_vcov(c(2, 1, 0.7), c(0.5, 0.3, 0.2))

    set.seed(1518)

    dat <- get_sim_data(n, sigma, trt = 8) %>%
        mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
        mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
        select(-is_miss) %>%
        mutate(group = factor(group, labels = c("Placebo", "TRT")))


    dat_ice <- dat %>%
        group_by(id) %>%
        arrange(id, visit) %>%
        filter(is.na(outcome)) %>%
        slice(1) %>%
        ungroup() %>%
        select(id, visit) %>%
        mutate(strategy = "JR")


    vars <- set_vars(
        outcome = "outcome",
        group = "group",
        strategy = "strategy",
        subjid = "id",
        visit = "visit",
        covariates = c("age", "sex", "visit * group")
    )
    list(dat = dat, dat_ice = dat_ice, vars = vars)
}






#########################
#
# Approximate Bayesian
#
#


set.seed(3123)
dobj <- get_data(40)

drawobj_ab <- draws(
    data = dobj$dat,
    data_ice = dobj$dat_ice,
    vars = dobj$vars,
    method = method_approxbayes(
        n_samples = 5,
        threshold = 0.5,
        same_cov = TRUE,
        REML = TRUE,
        covariance = "us"
    )
)

impute_ab <- impute(
    drawobj_ab,
    references = c("TRT" = "Placebo", "Placebo" = "Placebo"),
)

v2 <- dobj$vars
v2$covariates <- c("sex*age")
analysis_ab <- analyse(
    impute_ab,
    fun = ancova,
    vars = v2
)

pool_ab <- pool(analysis_ab, conf.level = 0.9, alternative = "less")

.test_print$approxbayes <- list(
    pool = pool_ab
)




#########################
#
# Bayesian
#
#


set.seed(413)
dobj <- get_data(40)

suppressWarnings({
    set.seed(859)
    drawobj_b <- draws(
        data = dobj$dat,
        data_ice = dobj$dat_ice,
        vars = dobj$vars,
        method = method_bayes(
            n_samples = 50,
            control = control_bayes(thin = 1)
        )
    )
})


impute_b <- impute(
    drawobj_b,
    references = c("TRT" = "TRT", "Placebo" = "Placebo"),
)

v2 <- dobj$vars
v2$covariates <- c("sex*age")
analysis_b <- analyse(
    impute_b,
    fun = rbmi::ancova,
    delta = delta_template(impute_b),
    visits = c("visit_1", "visit_3"),
    vars = v2
)

pool_b <- pool(analysis_b)


.test_print$bayes <- list(
    pool = pool_b
)




#########################
#
# Conditional Mean - Bootstrap
#
#

set.seed(313)
dobj <- get_data(40)

drawobj_cmb <- draws(
    data = dobj$dat,
    data_ice = dobj$dat_ice,
    vars = dobj$vars,
    method = method_condmean(
        n_samples = 5,
        threshold = 0.2,
        type = "bootstrap",
        same_cov = TRUE,
        REML = TRUE,
        covariance = "ar1"
    )
)

impute_cmb <- impute(
    drawobj_cmb,
    references = c("TRT" = "Placebo", "Placebo" = "Placebo"),
)

v2 <- dobj$vars
v2$covariates <- c("sex")
analysis_cmb <- analyse(
    impute_cmb,
    fun = ancova,
    vars = v2
)

pool_cmb_p <- pool(analysis_cmb, alternative = "greater")
pool_cmb_n <- pool(analysis_cmb, alternative = "greater", type = "normal")


.test_print$condmean_boot <- list(
    pool = list(
        percentile = pool_cmb_p,
        normal = pool_cmb_n
    )
)




#########################
#
# Conditional Mean - Jackknife
#
#

set.seed(89513)
dobj <- get_data(35)
drawobj_cmj <- draws(
    data = dobj$dat,
    data_ice = dobj$dat_ice,
    vars = dobj$vars,
    method = method_condmean(
        threshold = 0.5,
        same_cov = FALSE,
        REML = TRUE,
        type = "jackknife",
        covariance = "us"
    )
)

impute_cmj <- impute(
    drawobj_cmj,
    references = c("TRT" = "Placebo", "Placebo" = "Placebo"),
)


v2 <- dobj$vars
v2$covariates <- c("sex*age")
analysis_cmj <- analyse(
    impute_cmj,
    fun = ancova,
    vars = v2
)

pool_cmj <- pool(analysis_cmj, conf.level = 0.9)


.test_print$condmean_jack <- list(
    pool = pool_cmj
)


#########################
#
# BMLMI
#
#


set.seed(2413)
dobj <- get_data(40)

drawobj_bml <- draws(
    ncores = 1,
    data = dobj$dat,
    data_ice = dobj$dat_ice,
    vars = dobj$vars,
    method = method_bmlmi(
        covariance = "cs",
        threshold = 0.05,
        same_cov = TRUE,
        REML = TRUE,
        B = 6,
        D = 4
    )
)

impute_bml <- impute(
    drawobj_bml,
    references = c("TRT" = "Placebo", "Placebo" = "Placebo"),
)


v2 <- dobj$vars
v2$covariates <- c("sex*age")
analysis_bml <- analyse(
    impute_bml,
    fun = ancova,
    vars = v2
)

pool_bml <- pool(analysis_bml, conf.level = 0.9)

.test_print$bmlmi <- list(
    pool = pool_bml
)



usethis::use_data(.test_print, internal = TRUE, overwrite = TRUE)
