# Configure useragent to enable binary package installation from PPM
options(
    repos = c(
        CRAN = sprintf(
            "https://packagemanager.posit.co/cran/latest/bin/linux/noble-%s/%s",
            R.version["arch"],
            substr(getRversion(), 1, 3)
        )
    ),
    "HTTPUserAgent" = sprintf(
        "R/%s R (%s)",
        getRversion(),
        paste(
            getRversion(),
            R.version["platform"],
            R.version["arch"],
            R.version["os"]
        )
    )
)

# Throw errors if any warnings arise
options(warn = 2)

if (!requireNamespace("desc", quietly = TRUE)) {
    install.packages("desc")
}


# Get a list of all packages that we claim we depend on in the DESCRIPTION file
pkgs <- desc::desc("DESCRIPTION")$get_deps()$package |> unique()

# Add on additional packages that might be needed in the future
pkgs <- c(
    pkgs,
    "tidyverse",
    "devtools",
    "jinjar",
    "tidymodels",
    "languageserver",
    "fs",
    "assertthat",
    "lifecycle"
)


# R will error if you try to update the base packages so remove them
# if they are in any of the imports / suggests / depends list
base_pkgs <- c(
    "KernSmooth",
    "class",
    "foreign",
    "methods",
    "rpart",
    "survival",
    "MASS",
    "cluster",
    "grDevices",
    "mgcv",
    "spatial",
    "tcltk",
    "Matrix",
    "codetools",
    "graphics",
    "nlme",
    "splines",
    "tools",
    "base",
    "compiler",
    "grid",
    "nnet",
    "stats",
    "translations",
    "boot",
    "datasets",
    "lattice",
    "parallel",
    "stats4",
    "utils",
    "R"
)
pkgs <- pkgs[!pkgs %in% base_pkgs]

install.packages(pkgs, dependencies = TRUE)

# Install cmdstanr (not available from CRAN)
install.packages(
    "cmdstanr",
    repos = c("https://stan-dev.r-universe.dev", getOption("repos"))
)
