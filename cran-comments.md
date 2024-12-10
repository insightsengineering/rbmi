## Summary of Submission

This version of the package fixes an issue that was causing the unit tests to crash on CRAN. The issue was due to the package not correctly clearing out the cache of previously compiled `rstan` models.


## R CMD check results

There were no ERRORs, no WARNINGs and no NOTEs.


## Test environments

The package was tested in the following environments:

- MacOS, R release (Local Machine)
- Windows, R release (Win-Builder)
- MacOS, devel (macOS builder)
- Ubuntu 22.04 LTS, devel (Rhub / GitHub Actions)


## Downstream dependencies

There are currently no downstream dependencies for this package
