

# Determine which OS we are on
# Rocker images are Ubuntu based so should be jammy / focal / etc
OS_CODENAME <- system(
    'cat /etc/os-release | grep "VERSION_CODENAME" | sed -e "s/VERSION_CODENAME=//"',
    intern = TRUE
)

# Configure PPM URL for binary package installation
CRANURL <- sprintf(
    "https://packagemanager.posit.co/cran/__linux__/%s/latest",
    OS_CODENAME
)

# Configure useragent to enable binary package installation from PPM
options(
    "repos" = list("CRAN" = CRANURL),
    "HTTPUserAgent" = sprintf(
        "R/%s R (%s)",
        getRversion(),
        paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
    )
)

# Throw errors if any warnings arise
options(warn = 2)

if (!requireNamespace("desc", quietly = TRUE)) {
    install.packages("desc")
}

pkgs <- desc::desc("DESCRIPTION")$get_deps()$package |> unique()

# Add on additional packages that might be needed in the future
pkgs <- c(pkgs, "tidyverse")

install.packages(pkgs, dependencies = TRUE)

install.packages(
    "cmdstanr",
    repos = c("https://stan-dev.r-universe.dev", getOption("repos"))
)

cmdstanr::install_cmdstan(cores = 2)
