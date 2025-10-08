library(testthat)
library(rbmi)

# if (Sys.getenv("NOT_CRAN") != "true") {
#     Sys.setenv(RBMI_ENABLE_CACHE = "false")
#     options(rbmi.enable_cache = FALSE)
# }

test_check("rbmi")

# print(do.call(rbind, test_check("rbmi", testthat::ListReporter))[, c("file", "test", "real")])
