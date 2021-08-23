devtools::load_all()
library(dplyr)
library(glmmTMB)


sigma <- as_covmat(c(2, 1, 0.7), c(0.5, 0.3, 0.2))

set.seed(1518)

dat <- get_sim_data(500, sigma, trt = 8) %>%
    mutate(is_miss = rbinom(n(), 1, 0.5)) %>%
    mutate(outcome = if_else(is_miss == 1 & visit == "visit_3", NA_real_, outcome)) %>%
    select(-is_miss)


dat_ice <- dat %>%
    group_by(id) %>%
    arrange(id, visit) %>%
    filter(is.na(outcome)) %>%
    slice(1) %>%
    ungroup() %>%
    select(id, visit) %>%
    mutate(strategy = "JR")


vars <- ivars(
    outcome = "outcome",
    group = "group",
    strategy = "strategy",
    subjid = "id",
    visit = "visit",
    covariates = c("sex", "age")
)

time_it({
    drawobj <- draws(
        data = dat,
        data_ice = dat_ice,
        vars = vars,
        method = method_approxbayes(
            n_samples = 20,
            covariance = "toep"
        )
    )
})


attempt0 <- function(){
    glmmTMB(
        formula = outcome ~ 1 + group + visit + sex + age  + toep(0 + visit | id),
        data = dat,
        dispformula = ~0,
        REML = TRUE,
        control = glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "L-BFGS-B")
        )
    )
}

a0 <- attempt0()
start <- getME(a0, c("theta", "beta"))
start$beta <- start$beta[-length(start$beta)]
ld <- longDataConstructor$new(dat, vars)

attempt0s <- function(){
    glmmTMB(
        formula = outcome ~ 1 + group + visit + sex + age  + toep(0 + visit | id),
        data = ld$get_data(ld$sample_ids()),
        dispformula = ~0,
        REML = TRUE,
        start = start,
        control = glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "L-BFGS-B")
        )
    )
}


attempt1 <- function(){
    bdat <- ld$get_data(ld$sample_ids())
    mdf <- as_model_df(bdat, as_simple_formula(vars))
    smdf <- as_mmrm_df(mdf[-1], mdf$outcome, bdat$visit, bdat$id)
    glmmTMB(
        formula = as_mmrm_formula(smdf, "toep"),
        data = smdf,
        dispformula = ~0,
        REML = TRUE,
        control = glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "L-BFGS-B")
        )
    )
}

attempt1s <- function(){
    bdat <- ld$get_data(ld$sample_ids())
    mdf <- as_model_df(bdat, as_simple_formula(vars))
    smdf <- as_mmrm_df(mdf[-1], mdf$outcome, bdat$visit, bdat$id)
    glmmTMB(
        formula = as_mmrm_formula(smdf, "toep"),
        data = smdf,
        dispformula = ~0,
        REML = TRUE,
        start = start,
        control = glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "L-BFGS-B")
        )
    )
}


attempt2 <- function(){
    bdat <- ld$get_data(ld$sample_ids())
    mdf <- as_model_df(bdat, as_simple_formula(vars))
    smdf <- as_mmrm_df(mdf[-1], mdf$outcome, bdat$visit, bdat$id)
    glmmTMB(
        formula = as_mmrm_formula(smdf, "toep"),
        data = smdf,
        dispformula = ~0,
        REML = TRUE,
        control = glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "BFGS")
        )
    )
}

attempt2s <- function(){
    bdat <- ld$get_data(ld$sample_ids())
    mdf <- as_model_df(bdat, as_simple_formula(vars))
    smdf <- as_mmrm_df(mdf[-1], mdf$outcome, bdat$visit, bdat$id)
    glmmTMB(
        formula = as_mmrm_formula(smdf, "toep"),
        data = smdf,
        dispformula = ~0,
        REML = TRUE,
        start = start,
        control = glmmTMBControl(
            optimizer = optim,
            optArgs = list(method = "BFGS")
        )
    )
}


inner <- function(fun){
    x <- record(fun())
    if (length(x$warnings) > 1) {
        return(1)
    }
    x <- x$results
    if (x$fit$convergence == 0) {
        return(0)
    } else {
        return(1)
    }
}

runme <- function(fun) {
    tryCatch(
        inner(fun),
        error = function(e) { return(1) }
    )
}

time_it({
    r1 <- replicate(500, {
        runme(attempt1)
    })
})


time_it({
    r2 <- replicate(500, {
        runme(attempt1s)
    })
})

sprintf("%s / %s  (%4.1f %%)", sum(r1), length(r1), mean(r1) * 100)
sprintf("%s / %s  (%4.1f %%)", sum(r2), length(r2), mean(r2) * 100)




time_it({
    r1 <- replicate(50, {
        runme(attempt2)
    })
})


time_it({
    r2 <- replicate(50, {
        runme(attempt2s)
    })
})

sprintf("%s / %s  (%4.1f %%)", sum(r1), length(r1), mean(r1) * 100)
sprintf("%s / %s  (%4.1f %%)", sum(r2), length(r2), mean(r2) * 100)



bdat <- ld$get_data(ld$sample_ids())
mdf <- as_model_df(bdat, as_simple_formula(vars))
smdf <- as_mmrm_df(mdf[-1], mdf$outcome, bdat$visit, bdat$id)

time_it({ replicate(n = 20, {
x <- glmmTMB(
    formula = as_mmrm_formula(smdf, "toep"),
    data = smdf,
    dispformula = ~0,
    REML = TRUE,
    start = start,
    control = glmmTMBControl(
        optimizer = optim,
        optArgs = list(method = "L-BFGS-B")
    )
)
})})



# L-BFGS-B  with start = 8.15
# L-BFGS-B  without start = 8.42

# BFGS with start = 42.64
# BFGS without start = 64.26

record(attempt1s())
