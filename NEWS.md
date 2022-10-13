# rbmi (development version)

# rbmi 1.2.0

* Replaced our `glmmTMB` dependency with the `mmrm` package. This has resulted in the package being more stable (less model fitting convergance issues) as well as reducing run times to by about 2/3rds. 

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
  
