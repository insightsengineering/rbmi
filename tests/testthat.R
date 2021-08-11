library(testthat)
library(rbmi)
library(dplyr)

test_check("rbmi")

if(Sys.getenv("R_TEST_NIGHTLY") == "TRUE"){
    test_dir("nightly", stop_on_failure = TRUE)
}