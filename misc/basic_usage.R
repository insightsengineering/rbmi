

library(dplyr)
devtools::load_all()



sigma <- as_covmat(c(3, 4, 5), c(0.8, 0.6, 0.4))


set.seed(21)


dat <- get_sim_data(100, sigma) %>%
    mutate(is_miss = rbinom(n(), 1, 0.2)) %>%
    mutate(outcome = if_else(is_miss == 1, NA_real_, outcome)) %>%
    select(-is_miss)


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
    covariates = c("age", "sex", "visit*group")
)

drawobj <- draws(
    data = dat,
    data_ice = dat_ice,
    vars = vars,
    method = method_approxbayes(n_samples = 10)
)


imputeobj <- impute(
    draws = drawobj,
    references = c("A" = "B", "B" = "B")
)

vars2 <- vars
vars2$covariates <- c("age", "sex")

anaobj <- analyse(
    imputeobj,
    fun = rbmi::ancova,
    vars = vars2,
    visits = "visit_1"
)

poolobj <- pool(
    results = anaobj,
    conf.level = 0.95,
    alternative = "two.sided"
)





method_condmean(
    covariance = c("us", "toep", "cs", "ar1"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_samples = NULL,
    type = c("bootstrap", "jackknife")
)


method_bayes(
    burn_in = 200,
    burn_between = 50,
    same_cov = TRUE,
    n_samples = 20
)


method_approxbayes(
    covariance = c("us", "toep", "cs", "ar1"),
    threshold = 0.01,
    same_cov = TRUE,
    REML = TRUE,
    n_samples = 20
)





my_strategy <- function(pars_group, pars_ref, index_mar) {
    pars_group[["mu"]][index_mar] <- pars_ref[["mu"]][index_mar]
    return(pars_group)
}

imputeobj <- impute(
    draws = drawobj,
    references = c("A" = "B", "B" = "B"),
    update_strategy = update_dat_ice,
    strategies = getStrategies(
        "MS" = my_strategy
    )
)


myfun <- function(dat) {
    mod <- lm(data = dat, outcome ~ group)
    list(
        "treatment_effect" = list(
            "est" = coef(mod)[[2]],
            "se" = sqrt(vcov(mod)[2, 2]),
            "df" = df.residual(mod)
        )
    )
}

anaobj <- analyse(
    imputeobj,
    fun = myfun
)

res <- pool(anaobj)


imputeobj$longdata <- imputeobj$data

dat_delta <- delta_template(imputeobj) %>%
    mutate(delta = if_else(is_missing & group == "A", 3, 0)) %>%
    as_tibble()


anaobj <- analyse(
    imputeobj,
    fun = rbmi::ancova,
    vars = vars2,
    delta = dat_delta
)



expand_locf(
    data = dat,
    id = unique(dat$id),
    visit = unique(dat$visit),
    vars = c("sex", "age"),
    group = "id",
    order = c("id", "visit")
)
