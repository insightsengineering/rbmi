

devtools::load_all()

library(dplyr)

sigma <- as_vcov(
    c(5, 5, 6, 6, 8),
    c(0.3, 0.4, 0.45, 0.4, 0.3, 0.2, 0.1, 0.2, 0.3, 0.4)
)

dat <- get_sim_data(n = 300, sigma)


get_na_prob <- function(x){
    case_when(
        x == "visit_1" ~ 0,
        x == "visit_2" ~ 0.2,
        x == "visit_3" ~ 0.3,
        x == "visit_4" ~ 0.4,
        x == "visit_5" ~ 0.7
    )
}

dat2 <- dat %>%
    mutate(mprob = get_na_prob(visit)) %>%
    mutate(naact = runif(n())) %>%
    filter(naact > mprob) %>%
    select(-mprob, -naact)

dat3 <- expand_locf(
    data = dat2,
    id = unique(dat$id),
    visit = unique(dat$visit),
    fill_vars = c("age", "group", "sex"),
    fill_group = "id"
)

dat3 %>% filter(is.na(outcome))
dat3 %>% summarise(m = mean(is.na(outcome)) * 100)

vrs <- list(
    subjid = "id",
    visit = "visit",
    outcome = "outcome",
    strategy = "strategy",
    covariates = c("age", "sex", "age*sex", "visit*group"),
    group = "group"
)

dat_ice <- dat3 %>%
    arrange(id, visit) %>%
    group_by(id) %>%
    filter(row_number() == 1, is.na(outcome)) %>%
    ungroup() %>%
    select(id, visit) %>%
    mutate(strategy = "JR") %>%
    mutate(as.character(visit))


time_it({
    drawobj <- draws(
        data = dat3,
        data_ice = dat_ice,
        vars = vrs,
        method = method_approxbayes(n_samples = 20)
    )
})


time_it({
    drawobj <- draws(
        data = dat3,
        data_ice = dat_ice,
        vars = vrs,
        method = method_approxbayes(n_samples = 50)
    )
})



time_it({
    drawobj <- draws(
        data = dat3,
        data_ice = dat_ice,
        vars = vrs,
        method = method_approxbayes(n_samples = 100)
    )
})

time_it({
    drawobj <- as_class(drawobj, "random")

    imputeobj <- impute(
        drawobj,
        references = c("A" = "B", "B" = "B")
    )

    vrs2 <- vrs
    vrs2$covariates <- c("sex*age")
    anaobj <- analyse(
        imputations = imputeobj,
        fun = ancova,
        vars = vrs2,
        visit_level = "visit_5"
    )

    pool(anaobj)
})



### 500 subjects with 5 visits  with  2 covariates (and interaction) using JR
#
#  20 samples = 18sec  (0.9 sec per samp)
#  50 samples = 45sec  (0.9 sec per samp)
# 100 samples = 88sec  (0.9 sec per samp)
#



### 1000 subjects with 5 visits  with  2 covariates (and interaction) using JR
#
#  20 samples = 37sec   (1.9 sec per samp)
#  50 samples = 85sec   (1.7 sec per samp)
# 100 samples = 155sec  (1.5 sec per samp)
#
# The rest (impute/analyse/pool) on 100 samples = 13.2sec

