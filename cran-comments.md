## Summary of Submission

This is a re-submission to ensure that our unit tests do not fail on CRANs servers. 
The original uploads notes are as follows:

In this version I have:

* Replaced our dependencies from glmmTMB to mmrm to improve package performance and stability
* Upgraded our use of parallel processes to be more reliable in testing environments

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs:

❯ checking installed package size ... NOTE
    installed size is 57.4Mb
    sub-directories of 1Mb or more:
      libs  56.0Mb

- This is a consequence of using Rstan which produces quite large binaries when compiled. As far as I'm aware there is no way for us to reduce this and is dependent on the Stan development team. Our understanding from the [developers](https://discourse.mc-stan.org/t/using-rstan-in-an-r-package-generates-r-cmd-check-notes/26628) is that this is acceptable to ignore.


❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

- This is a consequence of using Rstan which requires us to use GNU make to compile the Stan model as per their package usage instructions which can be found [here](https://cran.r-project.org/web/packages/rstantools/vignettes/minimal-rstan-package.html).



## Test environments

The package was tested in the following environments:

- Ubuntu 20.04, R release (GitHub Actions)
- Windows latest, R release (Local Machine)
- Mac OS latest, R release (Local Machine + GitHub Actions)
- Ubuntu 20.04, R devel (GitHub Actions)


## Downstream dependencies

There are currently no downstream dependencies for this package
