mypkg <- basename(getwd())

# Get list of reverse dependencies (can toggle between recursive or not)
pkgs <- tools::dependsOnPkgs(
    mypkg,
    recursive = TRUE,
    dependencies = TRUE,
    installed = available.packages()
)

# Make sure we have all the development dependencies for each pkg we want
# to test and that all packages are at their latest version
install.packages(pkgs, dependencies = TRUE)
update.packages(ask = FALSE)

### Install our current package updates (run just 1 of the following)
### If using dev package be sure to reset to CRAN afterwards
# install.packages(mypkg)  # Use most recent CRAN pkg
devtools::install()        # Use current dev package

# Get source tars for each pkg to run r-cmd-check against
tmp_source <- tempfile()
dir.create(tmp_source, showWarnings = FALSE, recursive = TRUE)
source_files <- download.packages(pkgs = pkgs, destdir = tmp_source)

r_cmd_check <- function(pkg, pkg_src, outdir) {
    cat(sprintf("Checking %s...\n", pkg))
    cat(sprintf("  - %s\n\n", file.path(outdir, paste0(pkg, ".Rcheck"), "00check.log")))
    stdout_file <- tempfile()
    stderr_file <- tempfile()
    exit_status <- system2(
        "R",
        args = c("CMD", "check", pkg_src, sprintf("--output=%s", outdir)),
        stdout = stdout_file,
        stderr = stderr_file
    )
    stdout_output <- readLines(stdout_file)
    results <- grep("Status:", stdout_output, value = TRUE)
    unlink(stdout_file)
    cat(sprintf("%s\n", pkg))
    cat(sprintf("  - exit status %s\n", exit_status))
    cat(sprintf("  - %s\n", results))
    cat(sprintf("  - %s\n", file.path(outdir, paste0(pkg, ".Rcheck"))))
    cat(sprintf("  - %s\n\n", file.path(outdir, paste0(pkg, ".Rcheck"), "00check.log")))
    return(results)
}


cl <- parallel::makeCluster(7, outfile = "")
res <- parallel::clusterMap(
    cl,
    r_cmd_check,
    pkg = source_files[, 1],
    pkg_src = source_files[, 2],
    outdir = dirname(source_files[, 2])
)
parallel::stopCluster(cl)

# reset pkg back to current CRAN version
install.packages(mypkg)

cat("\n\n----  Done  ----\n\n")
