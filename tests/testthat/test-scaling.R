suppressPackageStartupMessages({
    library(dplyr)
})

test_that("scaler", {

    set.seed(101)
    mysig <- as_vcov(
        sd = c(1, 3, 5),
        cor = c(0.3, 0.5, 0.8)
    )

    dat_1 <- get_sim_data(1000, mysig)
    dat_2 <- get_sim_data(300, mysig)

    ####### Univariate scaling via LM

    mod_dat_1 <- as_model_df(dat_1, outcome ~ age + group + sex + visit)
    mod_dat_2 <- as_model_df(dat_2, outcome ~ age + group + sex + visit)

    scaler <- scalerConstructor$new(mod_dat_1)
    sdat <- scaler$scale(mod_dat_2)

    mod_original <- lm(data = dat_2, outcome ~ age + group + sex + visit)
    mod_scaled <- lm(outcome ~ .  - 1, data = sdat)

    expect_equal(
        coef(mod_original) %>% strip_names(),
        scaler$unscale_beta(coef(mod_scaled)) %>% strip_names()
    )

    expect_true(
        (ncol(sdat) - 1) == length(coef(mod_original) %>% strip_names())
    )

    expect_equal(
        summary(mod_original)$sigma^2 %>% strip_names(),
        scaler$unscale_sigma(summary(mod_scaled)$sigma^2) %>% strip_names()
    )


    ######## Multivariate via GLS


    mod_dat_2 <- as_model_df(dat_2, outcome ~ age + group + sex + visit)
    sdat <- scaler$scale(mod_dat_2)

    colnames(sdat) <- c("outcome", paste0("V", seq_len(ncol(sdat) - 1)))

    sdat$visit <- as.numeric(dat_2$visit)
    sdat$id <- dat_2$id

    mod_scaled <- nlme::gls(
        data = sdat,
        model = outcome ~ V1 + V2 + V3 + V4 + V5 + V6 - 1,
        correlation = nlme::corSymm(form = ~ visit | id),
        weights = nlme::varIdent(form = ~ 1 | visit),
        na.action = na.omit
    )

    mod_original <- nlme::gls(
        data = dat_2,
        model = outcome ~ age + group + sex + visit,
        correlation = nlme::corSymm(form = ~ as.numeric(visit) | id),
        weights = nlme::varIdent(form = ~ 1 | as.numeric(visit)),
        na.action = na.omit
    )

    expect_equal(
        coef(mod_original) %>% strip_names() %>% trunctate(5),
        scaler$unscale_beta(coef(mod_scaled)) %>% strip_names() %>% trunctate(5)
    )

    expect_true(
        (ncol(sdat) - 3) == length(coef(mod_original) %>% strip_names())
    )

    expect_equal(
        nlme::getVarCov(mod_original) %>% strip_names() %>% trunctate(2),
        scaler$unscale_sigma(nlme::getVarCov(mod_scaled)) %>% strip_names() %>% trunctate(2)
    )
})
