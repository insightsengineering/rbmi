
export RBMI_CACHE_DIR="$(pwd)/local"
export R_TEST_FULL=FALSE
export R_TEST_LOCAL=TRUE

echo "
Debug:
-------
PWD          = $(pwd)
R_TEST_FULL  = ${R_TEST_FULL}
R_TEST_LOCAL = ${R_TEST_LOCAL}

"

Rscript vignettes/build.R
