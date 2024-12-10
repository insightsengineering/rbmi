
# rbmi 1.3.1

* Fixed bug where stale caches of the `rstan` model were not being correctly cleared (#459)

# rbmi 1.3.0

## Breaking Changes

* Convert `rstan` to be a suggested package to simplify the installation process. This means that the Bayesian imputation functionality will not be available by default. To use this feature, you will need to install `rstan` separately (#441)
* Deprecated the `seed` argument to `method_bayes()` in favour of using the base `set.seed()` function (#431)

## New Features

* Added vignette on how to implement retrieved dropout models with time-varying intercurrent event (ICE) indicators (#414)
* Added vignette on how to obtain frequentist and information-anchored inference with conditional mean imputation using `rbmi` (#406)
* Added FAQ vignette including a statement on validation (#407 #440)
* Updates to `lsmeans()` for better consistency with the `emmeans` package (#412)
    * Renamed `lsmeans(..., weights = "proportional")` to `lsmeans(..., weights = "counterfactual")`to more accurately reflect the weights used in the calculation.
    * Added `lsmeans(..., weights = "proportional_em")` which provides consistent results with `emmeans(..., weights = "proportional")`
    * `lsmeans(..., weights = "proportional")` has been left in the package for backwards compatibility and is an alias for `lsmeans(..., weights = "counterfactual")` but now gives
    a message prompting users to use either "proptional_em" or "counterfactual" instead.
* Added support for parallel processing in the `analyse()` function (#370)
* Added documentation clarifying potential false-positive warnings from rstan (#288)
* Added support for all covariance structures supported by the `mmrm` package (#437)
* Updated `rbmi` citation detail (#423 #425)

## Miscellaneous Bug Fixes

* Stopped warning messages being accidentally supressed when changing the ICE type in `impute()` (#408)
* Fixed equations not rendering properly in the `pkgdown` website (#433) 


# rbmi 1.2.6

* Updated unit tests to fix false-positive error on CRAN's testing servers

# rbmi 1.2.5

* Updated internal Stan code to ensure future compatibility (@andrjohns, #390)
* Updated package description to include relevant references (#393)
* Fixed documentation typos (#393)

# rbmi 1.2.3

* Minor internal tweaks to ensure compatibility with the packages `rbmi` depends on

# rbmi 1.2.1

* Removed native pipes `|>` in testing code so package is backwards compatible with older servers
* Replaced our `glmmTMB` dependency with the `mmrm` package. This has resulted in the package being more stable (less model fitting convergence issues) as well as speeding up run times 3-fold. 

# rbmi 1.1.4

* Updated urls for references in vignettes
* Fixed a bug where visit factor levels were re-constructed incorrectly in `delta_template()`
* Fixed a bug where the wrong visit was displayed in the error message for when a specific visit doesn't have any data in `draws()`
* Fixed a bug where the wrong input parameter was displayed in an error message in `simulate_data()`

  
# rbmi 1.1.1 & 1.1.3
 
* No change in functionality from 1.1.0
* Various minor tweaks to address CRAN checks messages
  
# rbmi 1.1.0
  
* Initial public release
  
