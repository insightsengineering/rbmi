library(testthat)
library(rbmi)

if (Sys.getenv("NOT_CRAN") != "true") {
    Sys.setenv(RBMI_ENABLE_CACHE = "false")
    options(rbmi.enable_cache = FALSE)
}

test_check("rbmi")
