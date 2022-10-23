# rbmi (development version)

* `draws()` will now return the `mmrm` model fit object for the model fitted to the original
full dataset (after removing observations after a non-MAR ICE) if `method_condmean()` is used 
(Thank you @pengguanya)

# rbmi 1.2.1

* Removed native pipes `|>` in testing code so package is backwards compatible with older servers

# rbmi 1.2.0

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
  
