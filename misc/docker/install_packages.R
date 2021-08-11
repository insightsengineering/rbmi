
pkgs <- c(
    "tidyverse",
    "glmmTMB",
    "mvtnorm",
    "devtools",
    "rstan",
    "rstantools",
    "RcppParallel",
    "Rcpp",
    "R6",
    "assertthat",
    "emmeans",
    "covr",
    "testthat",
    "nlme",
    "roxygen2",
    "tinytex"
)

options(warn = 2)

install.packages(pkgs, repos = Sys.getenv("CRANURL"), dependencies = TRUE)

