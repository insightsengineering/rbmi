
#' @description
#'
#' The rbmi package is used to perform reference based multiple imputation. The package
#' provides implementations for common, patient-specific imputation strategies whilst allowing the user to
#' select between various standard Bayesian and frequentist approaches.
#'
#' The package is designed around its 4 core functions:
#' - [draws()] - Fits multiple imputation models
#' - [impute()] - Imputes multiple datasets
#' - [analyse()] - Analyses multiple datasets
#' - [pool()] - Pools multiple results into a single statistic
#'
#' To learn more about rbmi, please see the quickstart vignette:
#'
#' `vignette(topic= "quickstart", package = "rbmi")`
#'
#' @keywords internal
#' @importFrom assertthat  assert_that
"_PACKAGE"

