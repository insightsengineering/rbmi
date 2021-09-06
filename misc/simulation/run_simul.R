if(isFALSE(file.exists("Results"))) {
    dir.create("Results")
}

if(isFALSE(file.exists("Results/Res_each_sim"))) {
    dir.create("Results/Res_each_sim")
}

library(rbmi)
library(MASS)
library(dplyr)

run_simul <- function(H0) {

    source("simul_functions.R")

    # TODO: double check whether patients in the control arm are imputed under MAR or ref-based
    create_data_ice <- function(data, imp_drug, imp_placebo) {

        levels_visit <- levels(data$visit)
        J <- length(levels_visit)

        data_hasICE <- data %>%
            group_by(patnum) %>%
            mutate(hasICE = any(!trt_stop_visit %in% c(levels_visit[J], "Inf"))) %>%
            filter(hasICE) %>%
            select(-hasICE)

        data_ice <- data_hasICE %>%
            group_by(patnum) %>%
            summarise(
                "strategy" = ifelse(group[1] == "Intervention", imp_drug, imp_placebo),
                "visit" = levels_visit[which(trt_stop_visit[1] == levels_visit) + 1])

        return(data_ice)
    }

    impute_analyze_pool <- function(draws_obj, data_ice, references, vars, visit_analysis) {

        res_imp <- impute(
            draws = draws_obj,
            data_ice = data_ice,
            references = references
        )

        vars$covariates <- "y_bl"
        analyze_res <- analyse(
            imputations = res_imp,
            fun = ancova,
            vars = vars,
            visit_level = visit_analysis
        )

        if(class(draws_obj) == "condmean") {
            res <- pool(
                analyze_res,
                alternative = "two.sided",
                type = "normal"
            )
        } else {
            res <- pool(
                analyze_res,
                alternative = "two.sided"
            )
        }

        return(res)
    }

    # simulate data: 100 subjects per arm, assume a non-zero treatment effect
    data <- simul_data(n = 100, H0 = H0)

    factor_vars <- c("patnum", "group", "visit", "time", "trt_stop_visit", "dropout_visit", "time_off_trt")
    numeric_vars <- c("y_bl", "y_noICE", "y_noDropout", "y")
    data[,factor_vars] <- as.data.frame(lapply(data[,factor_vars], as.factor))
    data[,numeric_vars] <- as.data.frame(lapply(data[,numeric_vars], as.numeric))

    data_ice_MAR <- create_data_ice(data, "MAR", "MAR")
    data_ice_JR <- create_data_ice(data, "JR", "JR")
    data_ice_CR <- create_data_ice(data, "CR", "CR")
    data_ice_CIR <- create_data_ice(data, "CIR", "CIR")

    # remove baseline
    data <- data[data$visit != "0",]
    data$visit <- factor(data$visit, levels = levels(data$visit)[-1])
    levels_visit = levels(data$visit)
    J = nlevels(data$visit)

    # compute change from baseline
    data[,c("y_noICE", "y_noDropout", "y")] <- data[,c("y_noICE", "y_noDropout", "y")] - data$y_bl
    data <- as.data.frame(data)

    vars <- set_vars(
        outcome = "y",
        subjid = "patnum",
        visit = "visit",
        group = "group",
        covariates = c("y_bl*visit", "group*visit"),
        strategy = "strategy"
    )

    covariance <- "us"
    n_samples <- 1000
    n_imputations_bayes <- 100

    references <- c("Control" = "Control", "Intervention" = "Control")


    ############################# CONDITIONAL MEAN IMPUTATION (BOOTSTRAP)

    print("Running conditional mean imputation (bootstrap)")

    method <- method_condmean(
        covariance = covariance,
        n_samples = n_samples,
        type = "bootstrap"
    )

    draws_obj_bootstrap <- draws(
        data,
        data_ice = data_ice_JR,
        vars,
        method
    )

    ############## MAR
    res_bootstrap_MAR <- impute_analyze_pool(
        draws_obj_bootstrap,
        data_ice_MAR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )

    ############## JR
    res_bootstrap_JR <- impute_analyze_pool(
        draws_obj_bootstrap,
        data_ice_JR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )

    ############## CR
    res_bootstrap_CR <- impute_analyze_pool(
        draws_obj_bootstrap,
        data_ice_CR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )

    ############## CIR
    res_bootstrap_CIR <- impute_analyze_pool(
        draws_obj_bootstrap,
        data_ice_CIR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )



    ############################# CONDITIONAL MEAN IMPUTATION (JACKKNIFE)

    print("Running conditional mean imputation (jackknife)")

    method <- method_condmean(
        covariance = covariance,
        type = "jackknife"
    )

    draws_obj_jackknife <- draws(
        data,
        data_ice = data_ice_JR,
        vars,
        method
    )

    ############## MAR
    res_jackknife_MAR <- impute_analyze_pool(
        draws_obj_jackknife,
        data_ice_MAR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )

    ############## JR
    res_jackknife_JR <- impute_analyze_pool(
        draws_obj_jackknife,
        data_ice_JR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )

    ############## CR
    res_jackknife_CR <- impute_analyze_pool(
        draws_obj_jackknife,
        data_ice_CR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )

    ############## CIR
    res_jackknife_CIR <- impute_analyze_pool(
        draws_obj_jackknife,
        data_ice_CIR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )



    ############################# BAYESIAN MULTIPLE IMPUTATION

    method <- method_bayes(
        n_samples = n_imputations_bayes
    )

    draws_obj_bayesian <- draws(
        data,
        data_ice = data_ice_JR,
        vars,
        method
    )

    ############## MAR
    res_bayesian_MAR <- impute_analyze_pool(
        draws_obj_bayesian,
        data_ice_MAR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )

    ############## JR
    res_bayesian_JR <- impute_analyze_pool(
        draws_obj_bayesian,
        data_ice_JR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )

    ############## CR
    res_bayesian_CR <- impute_analyze_pool(
        draws_obj_bayesian,
        data_ice_CR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )

    ############## CIR
    res_bayesian_CIR <- impute_analyze_pool(
        draws_obj_bayesian,
        data_ice_CIR,
        references,
        vars,
        visit_analysis = levels_visit[J]
    )

    results <- list(
        "bootstrap" = list(
            res_MAR = res_bootstrap_MAR,
            res_JR = res_bootstrap_JR,
            res_CR = res_bootstrap_CR,
            res_CIR = res_bootstrap_CIR
        ),
        "jackknife" = list(
            res_MAR = res_jackknife_MAR,
            res_JR = res_jackknife_JR,
            res_CR = res_jackknife_CR,
            res_CIR = res_jackknife_CIR
        ),
        "bayesian" = list(
            res_MAR = res_bayesian_MAR,
            res_JR = res_bayesian_JR,
            res_CR = res_bayesian_CR,
            res_CIR = res_bayesian_CIR
        ),
        "n_failures" = list(
            bootstrap = draws_obj_bootstrap$n_failures,
            jackknife = draws_obj_jackknife$n_failures
        )
    )

    return(results)
}
# estimated time of one full simulation: 31 minutes
runsim <- function(i, H0) {
    xx <- run_simul(H0)
    saveRDS(c(i=i, H0=H0, xx), file = file.path('Results/Res_each_sim', paste('res', i, H0, '.rds', sep = '_')))
}


N <- 10000

parallel::mclapply(seq.int(N), runsim, H0=TRUE, mc.cores = parallel::detectCores())

li <- list.files('Results', pattern = '.*\\.rds$', full.names = TRUE)
saveRDS(bind_rows(lapply(li, function(X) unlist(readRDS(X)))), file = "results.rds")
