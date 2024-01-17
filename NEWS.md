
# rbmi (development version)

* Downgraded rstan to a `Suggests` dependency and opt to compile "just in time" rather than at installation time. This is intended to make installation easier for user who don't use `method_bayes()` (#405)

# rbmi 1.2.6

* Updated unit tests to fix false-positive error on CRAN's testing servers

# rbmi 1.2.5

* Updated internal Stan code to ensure future compatibility (@andrjohns, #390)
* Updated package description to include relevant references (#393)
* Fixed documentation typos (#393)

# rbmi 1.2.3

* Minor internal tweaks to ensure compatibility with the packages rbmi depends on

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
  
