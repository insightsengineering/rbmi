## Summary of Submission

This version (v1.2.6) adjusts our unit tests to fix a false positive test failure on CRANs
Clang Fedora/Debian servers.

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs:

❯ checking installed package size ... NOTE
  installed size is 55.6Mb
  sub-directories of 1Mb or more:
    libs  54.3Mb

- This is a consequence of using Rstan which produces quite large binaries when compiled. As far as I'm aware there is no way for us to reduce this and is dependent on the Stan development team. Our understanding from the [developers](https://discourse.mc-stan.org/t/using-rstan-in-an-r-package-generates-r-cmd-check-notes/26628) is that this is acceptable to ignore.


❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

- This is a consequence of using Rstan which requires us to use GNU make to compile the Stan model as per their package usage instructions which can be found [here](https://cran.r-project.org/web/packages/rstantools/vignettes/minimal-rstan-package.html).



## Test environments

The package was tested in the following environments:

- MacOS, R release (Local Machine)
- Fedora, R devel (Local Machine via Docker & Rhub)
- Debian, R release (Local Machine via Docker)
- Windows, R release (Win-Builder)
- Ubuntu, R release (GitHub Actions)

## Downstream dependencies

There are currently no downstream dependencies for this package
