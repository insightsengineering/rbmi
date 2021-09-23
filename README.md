<!-- badges: start -->
[![R-CMD-check](https://github.com/insightsengineering/rbmi/workflows/R-CMD-check/badge.svg)](https://github.com/insightsengineering/rbmi/actions)
[![Codecov test coverage](https://codecov.io/gh/insightsengineering/rbmi/branch/master/graph/badge.svg)](https://codecov.io/gh/insightsengineering/rbmi?branch=master)
<!-- badges: end -->


# Reference Based Multiple Imputation (rbmi)


## Overview

rbmi is an R package for performing reference based multiple imputation. The package
provides implementations for common, patient-specific imputation strategies whilst allowing the user to 
select between various standard Bayesian and frequentist approaches.

## Installation

The package can be installed directly from GitHub via:

```
devtools::install_github("insightsengineering/rbmi")
```

## Usage

The package is designed around its 4 core functions:

- `draws()` - Fits multiple imputation models
- `impute()` - Imputes multiple datasets
- `analyse()` - Analyses multiple datasets
- `pool()` - Pools multiple results into a single statistic

Full details including a walkthrough can be found in the quickstart vignette:

```
vignette(topic= "quickstart", package = "rbmi")
```

## Support

For any help with regards to using the package or if you find a bug please create a [GitHub issue](https://github.com/insightsengineering/rbmi/issues)
 
