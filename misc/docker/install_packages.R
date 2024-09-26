

OS_CODENAME <- system(
    'cat /etc/os-release | grep "VERSION_CODENAME" | sed -e "s/VERSION_CODENAME=//"',
    intern = TRUE
)

CRANURL <- sprintf(
    "https://packagemanager.posit.co/cran/__linux__/%s/latest",
    OS_CODENAME
)

options(
    "repos" = list("CRAN" = CRANURL),
    "HTTPUserAgent" = sprintf(
        "R/%s R (%s)",
        getRversion(),
        paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
    )
)

options(warn = 2)

pkgs <- c(
    "dplyr",
    "purrr",
    "tibble",
    "lubridate",
    "purrr",
    "mvtnorm",
    "devtools",
    "rstan",
    "rstantools",
    "RcppParallel",
    "Rcpp",
    "knitr",
    "R6",
    "assertthat",
    "rmarkdown",
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

install.packages(pkgs, dependencies = TRUE)

install.packages(
    "cmdstanr",
    repos = c("https://stan-dev.r-universe.dev", getOption("repos"))
)

cmdstanr::install_cmdstan(cores = 2)
