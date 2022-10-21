## Summary of Submission

In this version I have:

* Replaced our dependencies from glmmTMB to mmrm to improve package performance and stability
* Upgraded our use of parallel processes to be more reliable in testing environments

## R CMD check results

There were no ERRORs or WARNINGs.

There were 4 NOTEs:

❯ checking installed package size ... NOTE
    installed size is 57.4Mb
    sub-directories of 1Mb or more:
      libs  56.0Mb

- This is a consequence of using Rstan which produces quite large binaries when compiled. As far as I'm aware there is no way for us to reduce this and is dependent on the Stan development team. Our understanding from the [developers](https://discourse.mc-stan.org/t/using-rstan-in-an-r-package-generates-r-cmd-check-notes/26628) is that this is acceptable to ignore.


❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

- This is a consequence of using Rstan which requires us to use GNU make to compile the Stan model as per their package usage instructions which can be found [here](https://cran.r-project.org/web/packages/rstantools/vignettes/minimal-rstan-package.html).


> checking dependencies in R code ... NOTE
Namespaces in Imports field not imported from:
  ‘RcppParallel’ ‘rstantools’
  All declared Imports should be used.

- This is a false positive. These packages are used by the makefile which compiles the Stan code when the package is installed. My understanding is that the files containing these libraries are generated on the fly when the package is installed and are thus not visible to R CMD CHECK. Again this process is controlled by the Stan developers and I don't think there is anything we can do to resolve this. 


> checking R code for possible problems ... NOTE
Found the following calls to attach():
File ‘rbmi/R/parallel.R’:
  attach(getNamespace("rbmi"))
See section ‘Good practice’ in ‘?attach’.

- This is a false positive. We only use attach in a parallel process which is spawned and killed by our package. That is we do not cause any side effects on the users session. The relevant line of code can be found [here](https://github.com/insightsengineering/rbmi/blob/main/R/parallel.R#L27)




## Test environments

The package was tested in the following environments:

- Ubuntu 20.04, R release (GitHub Actions)
- Windows latest, R release (Local Machine)
- Mac OS latest, R release (Local Machine + GitHub Actions)
- Ubuntu 20.04, R devel (GitHub Actions)


## Downstream dependencies

There are currently no downstream dependencies for this package
