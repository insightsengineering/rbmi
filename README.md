<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/rbmi)](https://cran.r-project.org/package=rbmi)
[![R-CMD-check](https://github.com/insightsengineering/rbmi/actions/workflows/on_push.yaml/badge.svg)](https://github.com/insightsengineering/rbmi/actions/workflows/on_push.yaml)
<!-- badges: end -->


# Reference Based Multiple Imputation (rbmi)


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

## Support

For any help with regards to using the package or if you find a bug please create a [GitHub issue](https://github.com/insightsengineering/rbmi/issues)
 
