## Summary of Submission

In this version I have:

* Implemented minor bug fixes
* Cleaned up documentation

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs:

❯ checking installed package size ... NOTE
    installed size is 57.4Mb
    sub-directories of 1Mb or more:
      libs  56.0Mb

❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

Both of the above notes are a consequence of using rstan in the package following the usage steps as described by the stan developers [here](https://cran.r-project.org/web/packages/rstantools/vignettes/minimal-rstan-package.html). Our understanding from the [developers](https://discourse.mc-stan.org/t/using-rstan-in-an-r-package-generates-r-cmd-check-notes/26628) is that they are acceptable to ignore.

## Test environments

The package was tested in the following environments:

- Ubuntu 20.04, R release (GitHub Actions)
- Windows latest, R release (Local Machine)
- Mac OS latest, R release (Local Machine + GitHub Actions)
- Ubuntu 20.04, R devel (GitHub Actions)


## Downstream dependencies

There are currently no downstream dependencies for this package
