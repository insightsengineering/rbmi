









suppressPackageStartupMessages({
    library(dplyr)
})

test_that("scaler", {

    set_col_names <- function(x, nam) {
        colnames(x) <- nam
        return(x)
    }

    f2n <- function(x) as.numeric(x) - 1

    as_covmat <- function(sig, corr) {
        len <- length(sig)
        cormat <- diag(rep(1, len))
        index <- 1
        for (i in 1:len) {
            for (j in 1:len) {
                if (i < j) {
                    cormat[i, j] <- corr[index]
                    cormat[j, i] <- corr[index]
                    index <- index + 1
                }
            }
        }
        return((sig %*% t(sig)) * cormat)
    }

    strip_names <- function(x) {
        names(x) <- NULL
        colnames(x) <- NULL
        rownames(x) <- NULL
        return(x)
    }

    trunctate <- function(x, n) {
        floor(x * 10 ^ n) / 10 ^ n
    }

    n <- 500
    nv <- 3

    set.seed(101)

    covars <- tibble(
        id = 1:n,
        age = rnorm(n),
        group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
        sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F"))
    )

    mysig <- as_covmat(
        sig = c(1, 3, 5),
        corr = c(0.3, 0.5, 0.8)
    )

    dat_1 <- mvtnorm::rmvnorm(n, sigma = mysig) %>%
        set_col_names(paste0("visit_", 1:nv)) %>%
        as_tibble() %>%
        mutate(id = 1:n()) %>%
        tidyr::gather("visit", "outcome", -id) %>%
        mutate(visit = factor(visit)) %>%
        arrange(id, visit) %>%
        left_join(covars, by = "id") %>%
        mutate(outcome = outcome + 5 +  3 * age + 3 * f2n(sex) + 4 * f2n(group)) %>%
        mutate(id = as.character(id))

    dat_2 <- mvtnorm::rmvnorm(n, sigma = mysig) %>%
        set_col_names(paste0("visit_", 1:nv)) %>%
        as_tibble() %>%
        mutate(id = 1:n()) %>%
        tidyr::gather("visit", "outcome", -id) %>%
        mutate(visit = factor(visit)) %>%
        arrange(id, visit) %>%
        left_join(covars, by = "id") %>%
        mutate(outcome = outcome + 5 +  3 * age + 3 * f2n(sex) + 4 * f2n(group)) %>%
        mutate(id = as.character(id))



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
