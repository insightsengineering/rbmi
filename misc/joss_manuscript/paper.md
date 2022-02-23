---
title: 'rbmi: A R package for standard and reference-based multiple imputation methods'
tags:
  - R
  - Biostatistics
  - Clinical Trials
  - Estimands
  - Missing Data
  - Multiple Imputation
  - Reference-based Methods
authors:
  - name: Craig Gower-Page
    affiliation: 1
  - name: Alessandro Noci
    affiliation: 2
  - name: Marcel Wolbers
    affiliation: 2

affiliations:
 - name: Data and Statistical Sciences, Pharma Development, Roche, Welwyn Garden City, UK
   index: 1
 - name: Data & Statistical Sciences, Pharma Development, Roche, Basel, Switzerland
   index: 2
citation_author: Gower-Page, Noci, and Wolbers
date: \today
bibliography: references.bib
output: rticles::joss_article
journal: JOSS
---



# Summary 

Many randomized controlled clinical trials compare a continuous outcome variable which is assessed longitudinally at scheduled follow-up visits between subjects assigned to the intervention treatment group and those assigned to the control group. 
Missing outcome measurements may occur because subjects miss an assessment or drop out from the trial altogether. 
Moreover, intercurrent events (ICEs) such as discontinuations of the assigned treatment or initiations of rescue medications may affect the interpretation or the existence of the outcome measurements associated with the clinical question of interest. The ICH E9(R1) addendum on estimands, a regulatory document published by the International Council for Harmonisation of Technical Requirements for Pharmaceuticals for Human Use, presents a structured framework to link trial objectives to a precise description of the targeted treatment effect in the presence of ICEs and missing data [@iche9r1].

The R package `rbmi` was created to support trial analyses which are aligned with the estimands framework.
Missing data is handled using multiple imputation (MI) assuming multivariate normally distributed data. 
The package supports both standard imputation under a missing-at-random assumption and reference-based imputation methods. Reference-based methods impute missing data in the intervention treatment group based on observed data from the control group [@CarpenterEtAl2013]. $\delta$-based imputation methods which add an offset term, $\delta$, to the imputed values prior to the analysis in order to assess the impact of unobserved outcomes being worse or better than those observed are also supported. Such methods are frequently used for sensitivity or "tipping point" analyses [@CroEtAlTutorial2020]. 


# Statement of need 

`rbmi` is a flexible `R` package designed to support the analysis of randomized clinical trials with continuous longitudinal endpoints. 
Both conventional MI methods based on Bayesian posterior draws and novel methods based on maximum likelihood estimation and re-sampling (as decribed in @vanHippelBartlett2021 and @Wolbers2021) are implemented. `rbmi` was designed for statisticians from both academic clinical research units and pharmaceutical industry. To our knowledge, a comprehensive and fully validated `R` implementation of such approaches is still lacking. An established software implementation of reference-based imputation in SAS are the so-called "five macros" [@FiveMacros]. An alternative `R` implementation which is currently under development is the R package `RefBasedMI`[@RefbasedMIpackage].

# Implementation 

All approaches implemented in `rbmi` follow a common workflow based on 4 core functions which are called sequentially: 

- `draws()` - fits the imputation models and stores their parameters
- `impute()` - creates multiple imputed datasets
- `analyse()` - analyses each of the multiple imputed datasets
- `pool()` - combines the analysis results across imputed datasets into a single statistic

This modular design creates a user-friendly and extensible environment that allow the user to have a direct control on all the phases of the estimation process. 
In addition, a variety of helper functions have been implemented to further support the user.

The `draws()` function has 3 input arguments: 

- `data`: The primary longitudinal `data.frame` containing the outcome variable and all covariates. The inclusion of time-varying covariates is also possible.
- `data_ice`: A `data.frame` which specifies the first visit affected by an ICE and the imputation strategy for handling missing outcome data after the ICE.
- `method`: The selected statistical approach which is defined by creating a `method` object by using one of:
    - `method_bayes()` for MI based on Bayesian posterior parameter draws from MCMC sampling and inference based on Rubin's rules [@CarpenterEtAl2013].
    - `method_approxbayes()`: as for `method_bayes()` except that  approximate Bayesian posterior draws are obtained via bootstrapping and maximum likelihood estimation (@LittleRubin1992[Section 10.2.3, part 6]).
    - `method_condmean()` for conditional mean imputation based on maximum likelihood estimation. Inference is based on re-sampling techniques (bootstrap or jackknife) as described in @Wolbers2021.
    - `method_bmlmi()` for bootstrapped maximum likelihood MI as described in @vanHippelBartlett2021.

In addition to detailed help files for all functions, the package contains three vignettes: a `quickstart` vignette which describes the basic functionality, an `advanced` vignette which describes some of the advanced features, and a `stat_specs` vignette which describes the statistical methodology in detail. 

# Development and implementation 

`rbmi` is developed open source on https://github.com/insightsengineering/rbmi and major releases will also be uploaded to [CRAN](https://cran.r-project.org/).
All production code is required to have been reviewed by an independent programmer as well as pass a  suite of automated unit tests which both define and document the expected input and output of each function.
These practices ensure that the package is of the highest standard and performs as expected.
Additionally comparisons are made to similar software (namely the so-called "five macros" [@FiveMacros] SAS implementation) to ensure consistency of results as well as to simulated datasets with known values.
To date, `rbmi` has been used in two simulation studies reported in @Wolbers2021 and @Noci2021.


# Author contributions and acknowledgements

Craig Gower-Page and Alessandro Noci are the primary developers of the `rbmi` package. Marcel Wolbers initiated the project (jointly with Paul Delmar), specified the statistical methods and contributed to the documentation and vignettes. 

The authors thank Jonathan Bartlett from the University of Bath and Paul Delmar and Daniel Sabanés Bové from Roche for many helpful discussions on the statistical methodology and the software implementation.

# References

