


options(warn = 2)

install.packages("remotes", repos = Sys.getenv("CRANURL"))

# mmrm wasn't available when in-house servers locked their package versions
# and instead had v0.13 of mmrm patched in after the fact
# here we allow the build to specify "latest" to grab the latest version of mmrm
# or a specific named version from github
mmrm_version <- Sys.getenv("MMRM_VERSION")
if (mmrm_version == "latest") {
    install.packages("mmrm", repos = Sys.getenv("CRANURL"), dependencies = TRUE)
} else {
    remotes::install_git("https://github.com/openpharma/mmrm.git", ref = mmrm_version)
}

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
    "tinytex",
    "bookdown",
    "RhpcBLASctl",
    "R.rsp"
)

install.packages(pkgs, repos = Sys.getenv("CRANURL"), dependencies = TRUE)
