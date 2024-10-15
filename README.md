<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/rbmi)](https://cran.r-project.org/package=rbmi)
[![R-CMD-check](https://github.com/insightsengineering/rbmi/actions/workflows/on_push.yaml/badge.svg?branch=main)](https://github.com/insightsengineering/rbmi/actions/workflows/on_push.yaml)
<!-- badges: end -->

# Reference Based Multiple Imputation (`rbmi`) <a href='https://insightsengineering.github.io/rbmi/'><img src="man/figures/logo.png" align="right" height="139" style="max-width: 100%; max-height: 139px;"/></a  >


## Overview

rbmi is a R package for imputation of missing data in clinical trials with continuous multivariate normal longitudinal outcomes. 
It supports imputation under a missing at random (MAR) assumption, reference-based imputation methods, 
and delta adjustments (as required for sensitivity analysis such as tipping point analyses). The package implements both Bayesian and 
approximate Bayesian multiple imputation combined with Rubin's rules for inference, and frequentist conditional mean imputation combined with 
(jackknife or bootstrap) resampling. 

## Installation

The package can be installed directly from CRAN via:

```
install.packages("rbmi")
```

Note that the usage of Bayesian multiple imputation requires the installation of the suggested 
package [rstan](https://CRAN.R-project.org/package=rstan).
```
install.packages("rstan")
```

## Usage

The package is designed around its 4 core functions:

- `draws()` - Fits multiple imputation models
- `impute()` - Imputes multiple datasets
- `analyse()` - Analyses multiple datasets
- `pool()` - Pools multiple results into a single statistic

The basic usage of these core functions is described in the quickstart vignette:

```
vignette(topic = "quickstart", package = "rbmi")
```

## Validation

For clarification on the current validation status of rbmi please see the FAQ vignette.


## Support

For any help with regards to using the package or if you find a bug please create a [GitHub issue](https://github.com/insightsengineering/rbmi/issues)
 
