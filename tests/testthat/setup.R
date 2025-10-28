# Run before any test
#
# Note:
#   - This file will be loaded by devtools::test()
#   - This file will NOT be loaded by devtools::load_all()
#
# https://testthat.r-lib.org/articles/special-files.html#setup-files
#
if (is_local_test()) {
    old_cache <- getOption("rbmi.cache_dir")
    tmp_dir <- tempfile(tmpdir = tempdir(check = TRUE))
    dir.create(tmp_dir)
    options("rbmi.cache_dir" = tmp_dir)

    # Run after all tests
    withr::defer(
        {
            unlink(tmp_dir, recursive = TRUE)
            options("rbmi.cache_dir" = old_cache)
        },
        teardown_env()
    )
} else {
    options("rbmi.enable_cache" = FALSE)
}
