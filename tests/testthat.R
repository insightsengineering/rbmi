library(testthat)
library(rbmi)

test_check("rbmi")

if(!Sys.getenv("R_TEST_NIGHTLY") == "TRUE"){
    test_dir("nightly", stop_on_failure = TRUE)
}