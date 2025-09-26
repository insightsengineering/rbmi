library(testthat)
library(rbmi)

# if (Sys.getenv("NOT_CRAN") != "true") {
#     Sys.setenv(RBMI_ENABLE_CACHE = "false")
#     options(rbmi.enable_cache = FALSE)
# }



report_list <- test_check("rbmi", testthat::ListReporter)

print(do.call(rbind, report_list)[, c("file", "test", "real")])
