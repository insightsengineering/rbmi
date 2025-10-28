
unset RBMI_CACHE_DIR
export R_TEST_FULL=FALSE
export R_TEST_LOCAL=FALSE

echo "
Debug:
-------
PWD          = $(pwd)
R_TEST_FULL  = ${R_TEST_FULL}
R_TEST_LOCAL = ${R_TEST_LOCAL}

"

PKGDIR=$(pwd)
temp_dir=$(mktemp -d)
cd ${temp_dir}

R CMD build ${PKGDIR}
R CMD check --as-cran --output=$temp_dir $temp_dir/*.tar.gz
