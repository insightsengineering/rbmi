# Run before any test
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
