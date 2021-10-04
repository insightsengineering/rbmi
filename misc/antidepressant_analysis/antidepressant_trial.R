if(isFALSE(file.exists("Results"))) {
  dir.create("Results")
}

library(rbmi)
library(dplyr)

set.seed(101)
# double check whether patient in the control arm are imputed under MAR or ref-based
# it should not make any difference since all post-ICE data are missing
create_data_ice <- function(data, imp_drug, imp_placebo) {

  data_NA <- data %>%
    group_by(PATIENT) %>%
    mutate(allNA = any(is.na(change))) %>%
    filter(allNA) %>%
    select(-allNA)

  data_ice <- data_NA %>%
    group_by(PATIENT) %>%
    summarise("strategy" = ifelse(THERAPY[1] == "DRUG", imp_drug, imp_placebo),
              "VISIT" = as.character(levels(data$VISIT)[is.na(change)][1])
    )
  # patient with id 3618 is the unique one that has an intermittent missing values
  # actually he does not stop the treatment -> remove from data_ice
  data_ice <- data_ice[-which(data_ice$PATIENT == 3618),]

  return(data_ice)
}

impute_analyze_pool <- function(draws_obj, data_ice, references, vars, visit_analysis) {

  res_imp <- impute(
    draws = draws_obj,
    update_strategy = data_ice,
    references = references
  )

  vars$covariates = "basval"
  analyze_res <- analyse(
    imputations = res_imp,
    fun = ancova,
    vars = vars,
    visits = visit_analysis
  )

  if(class(draws_obj)[2] == "condmean") {
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

data <- haven::read_sas("chapter15_example.sas7bdat")
data[,c("PATIENT", "VISIT", "THERAPY")] <- as.data.frame(lapply(data[,c("PATIENT", "VISIT", "THERAPY")], as.factor))
data[,c("basval", "change")] <- as.data.frame(lapply(data[,c("basval", "change")], as.numeric))


run_analysis <- function(data, n_samples_boot, n_samples_bayes) {

  vars <- set_vars(
    outcome = "change",
    subjid = "PATIENT",
    visit = "VISIT",
    group = "THERAPY",
    covariates = c("basval*VISIT", "THERAPY*VISIT"),
    strategy = "strategy"
  )

  covariance <- "us"

  levels_visit = levels(data$VISIT)
  J = nlevels(data$VISIT)

  data_exp <- expand(
    data,
    PATIENT = unique(data$PATIENT),
    VISIT = levels_visit
  )

  data_exp <- fill_locf(
    data_exp,
    vars = c("basval", "THERAPY"),
    group = vars$subjid,
    order = vars$visit
  )

  data_ice_MAR <- create_data_ice(
    data = data_exp,
    imp_drug = "MAR",
    imp_placebo = "MAR"
  )

  data_ice_JR <- create_data_ice(
    data = data_exp,
    imp_drug = "JR",
    imp_placebo = "JR"
  )

  data_ice_CR <- create_data_ice(
    data = data_exp,
    imp_drug = "CR",
    imp_placebo = "CR"
  )

  data_ice_CIR <- create_data_ice(
    data = data_exp,
    imp_drug = "CIR",
    imp_placebo = "CIR"
  )

  references <- c("DRUG" = "PLACEBO", "PLACEBO" = "PLACEBO")



  ############################# CONDITIONAL MEAN IMPUTATION (BOOTSTRAP)

  method <- method_condmean(
    covariance = covariance,
    n_samples = n_samples_boot,
    type = "bootstrap"
  )

  init <- Sys.time()
  draws_obj_bootstrap <- draws(
    data_exp,
    data_ice = data_ice_JR,
    vars,
    method
  )
  end <- Sys.time()
  time_draws_bootstrap <- end - init

  ############## MAR
  init <- Sys.time()
  res_bootstrap_MAR <- impute_analyze_pool(
    draws_obj_bootstrap,
    data_ice_MAR,
    references,
    vars,
    visit_analysis = levels_visit[J]
  )
  end <- Sys.time()
  time_imp_an_pool_bootstrap <- end - init

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

  method <- method_condmean(
    covariance = covariance,
    type = "jackknife"
  )

  init <- Sys.time()
  draws_obj_jackknife <- draws(
    data_exp,
    data_ice = data_ice_JR,
    vars,
    method
  )
  end <- Sys.time()
  time_draws_jackknife <- end - init

  ############## MAR
  init <- Sys.time()
  res_jackknife_MAR <- impute_analyze_pool(
    draws_obj_jackknife,
    data_ice_MAR,
    references,
    vars,
    visit_analysis = levels_visit[J]
  )
  end <- Sys.time()
  time_imp_an_pool_jackknife <- end - init

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
    n_samples = n_samples_bayes
  )

  init <- Sys.time()
  draws_obj_bayesian <- draws(
    data_exp,
    data_ice = data_ice_JR,
    vars,
    method
  )
  end <- Sys.time()
  time_draws_bayesian <- end-init

  ############## MAR
  init <- Sys.time()
  res_bayesian_MAR <- impute_analyze_pool(
    draws_obj_bayesian,
    data_ice_MAR,
    references,
    vars,
    visit_analysis = levels_visit[J]
  )
  end <- Sys.time()
  time_imp_an_pool_bayesian <- end-init

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
    )
  )

  draws_obj <- list(
    "bootstrap" = draws_obj_bootstrap,
    "jackknife" = draws_obj_jackknife,
    "bayesian" = draws_obj_bayesian
  )

  times <- list(
    "bootstrap" = c(
      "draws" = time_draws_bootstrap,
      "imp_an_pool" = time_imp_an_pool_bootstrap
    ),
    "jackknife" = c(
      "draws" = time_draws_jackknife,
      "imp_an_pool" = time_imp_an_pool_jackknife
    ),
    "bayesian" = c(
      "draws" = time_draws_bayesian,
      "imp_an_pool" = time_imp_an_pool_bayesian
    )
  )

  ret_obj <- list(
    "results" = results,
    "draws_obj" = draws_obj,
    "times" = times
  )

  return(ret_obj)
}

ret_obj <- run_analysis(
  data = data,
  n_samples_boot = 10000,
  n_samples_bayes = 1000
)

saveRDS(ret_obj$results, file = "Results/results3.rds")
saveRDS(ret_obj$draws_obj, file = "Results/draws_obj3.rds")
saveRDS(ret_obj$times, file = "Results/times3.rds")

# results can be seen at Wolbers et al 2021 (https://arxiv.org/abs/2109.11162, table 1)
