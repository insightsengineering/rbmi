export _R_CHECK_FORCE_SUGGESTS_ FALSE
export LC_CTYPE en_GB.UTF-8
export RMPI_TYPE OPENMPI
export R_BROWSER false
export R_PDFVIEWER false
export OMP_NUM_THREADS 2
export _R_CHECK_INSTALL_DEPENDS_ true
#export _R_CHECK_SUGGESTS_ONLY_ true
export _R_CHECK_NO_RECOMMENDED_ true
export _R_CHECK_TIMINGS_ 10
export _R_CHECK_DEPRECATED_DEFUNCT_ true
export _R_CHECK_CODE_ASSIGN_TO_GLOBALENV_ true
export _R_CHECK_CODE_DATA_INTO_GLOBALENV_ true
export _R_CHECK_SCREEN_DEVICE_ warn
export _R_CHECK_S3_METHODS_NOT_REGISTERED_ true
export _R_CHECK_OVERWRITE_REGISTERED_S3_METHODS_ true
export _R_CHECK_NATIVE_ROUTINE_REGISTRATION_ true
export _R_CHECK_FF_CALLS_ registration
export _R_CHECK_PRAGMAS_ true
export _R_CHECK_COMPILATION_FLAGS_ true
export _R_CHECK_COMPILATION_FLAGS_KNOWN_ "-Wconversion -Wno-sign-conversion"
export _R_CHECK_THINGS_IN_TEMP_DIR_ true
export _R_CHECK_THINGS_IN_TEMP_DIR_EXCLUDE_ "^ompi"
export _R_CHECK_MATRIX_DATA_ TRUE

export OMP_THREAD_LIMIT 2
export R_DEFAULT_INTERNET_TIMEOUT 600
export NOAWT 1
export RGL_USE_NULL true
export WNHOME /usr/local/wordnet-3.1


temp_dir=$(mktemp -d)
mkdir $temp_dir
R CMD BUILD .
mv rbmi*.tar.gz $temp_dir
R CMD CHECK --output=$temp_dir $temp_dir/rbmi*.tar.gz

rm -rf $temp_dir
