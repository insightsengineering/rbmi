--- 
title: 'rbmi: A R package for reference-based multiple imputation methods'
author: Alessandro Noci[^1], Craig Gower-Page[^1], Marcel Wolbers[^1]
tags:
  - R
  - Biostatistics
  - Clinical Trials
  - Estimands
  - Missing Data
  - Multiple Imputation
  - Reference-based Methods
output: 
  bookdown::pdf_book:
    toc: false
    citation_package: natbib
date: \today
bibliography: references.bib
link-citations: true
---

[^1]: Affiliation: F.Hoffmann La-Roche

# Summary

In clinical trials the estimand framework is a systematic approach to define in detail what needs to be estimated to address the scientific question of interest posed by the trial objective. This framework has been proposed in the ICH E9(R1) addendum on estimands and sensitivity analyses to ensure alignment among clinical trial objectives, trial execution/conduct, statistical analyses, and interpretation of results [@iche9r1].

Once the estimand has been defined, the evaluation of the efficacy of the treatment under investigation requires the definition of an estimator which is well-aligned with the estimand. For more than a decade a mixed effects model repeated measures (MMRM) approach has been the de facto standard for the primary analysis when the primary outcome is a continuous longitudinal variable. However, it is increasingly recognized that alternative statistical methods to MMRM may be required in order to properly align the analysis method with the targeted estimand. A recent overview of methods to align the estimator with the estimand is @Mallinckrodt2020. A short introduction on estimation methods for studies with longitudinal endpoints can also be found in @Wolbers2021. One prominent statistical method for this purpose is multiple imputation (MI), which is the target of the `rbmi` package. 

Methods based on MI are an attractive and flexible alternative to MMRM. They allow for the imputation of missing data under both missing-at-random (MAR) and missing-not-at-random (MNAR) assumptions. For example, they allow imputation of missing data after a treatment discontinuation in the active arm based on data from the control arm (so-called reference-based imputation). Different reference-based assumptions can be adopted depending on the disease area and the anticipated mechanism of action of the intervention. For a general description and review of reference-based imputation methods, we refer to @CarpenterEtAl2013, @CroEtAlTutorial2020, and @Wolbers2021.

The ICH E9(R1) addendum also stresses the importance of performing proper sensitivity analyses to assess how robust the results given by the primary analysis are [@iche9r1]. Sensitivity analyses should also be planned to evaluate the impact of the modeling assumptions about missing data. For example, one could penalize the mean outcome of subjects with unobserved data by a pre-specified fixed amount. This kind of approach is called $\delta$-adjustment [@CroEtAlTutorial2020], where $\delta$ refers to the amount of the penalization. This method can be used to perform a "tipping-point analysis", which consists in progressively increasing the amount of $\delta$ adjustment from 0 until the conclusions from the primary analysis are overturned. Sensitivity analyses should also be performed for MI-based estimators and `rbmi` supports this.

# Statement of need

`rbmi` is a flexible R package designed to support the analysis of randomized clinical trials with continuous longitudinal endpoints. It allows for imputation under missing-at-random and under reference-based methods. Both conventional MI methods based on Bayesian posterior draws and novel methods based on conditional mean imputation and re-sampling are supported. `rbmi` also includes the possibility of performing sensitivity analyses based on fixed $\delta$-adjustments. `rbmi` was designed for statisticians from both academic research units and pharmaceutical industry who deal with late-phase randomized clinical trials. `rbmi` could be used for regulatory purposes or in simulation studies. To our knowledge, a comprehensive, reliable and fully validated implementation of such approaches that also serves for regulatory purposes is still missing. An established software implementation of reference-based imputation in SAS are the so-called "five macros" by James Roger [@FiveMacros]. An alternative `R` implementation which is also currently under development is the R package `RefBasedMI`[@RefbasedMIpackage].

# Implementation

All the approaches implemented in `rbmi` follow a common workflow based on 4 steps whose corresponding functions should be called sequentially. Additionally, helper functions that simplify the set-up of the input parameters and the interpretation of the outputs have also been implemented. This creates a user-friendly environment that allow the user to have a direct control on all the phases of the estimation process. The 4 core functions are the following:

- `draws()` - fits the imputation models and stores their parameters.
- `impute()` - creates multiple imputed datasets.
- `analyse()` - analyses each of the multiple imputed datasets.
- `pool()` - combines the analysis results across imputed datasets into a single statistic.

The different approaches can be set by defining a `method` object using the functions:

- `method_bayes()` for conventional Bayesian MI based on MCMC sampling and random imputation. Inference is based on Rubin's rules (@Barnard1999, @LittleRubin1992). This method is described in @CarpenterEtAl2013 and @CroEtAlTutorial2020.
- `method_approxbayes()` for approximate Bayesian MI where posterior draws are obtained via bootstrap instead of via MCMC (@LittleRubin1992, @Efron1994, @Honaker2010, @vanHippelBartlett2021).
- `method_condmean()` for maximum likelihood parameter estimation and conditional mean imputation. Inference is based on re-sampling techniques (bootstrap [@EfronTibs1994] or jackknife [@Efron1981]). This method is described in @Wolbers2021.
- `method_bmlmi()` for bootstrapped maximum likelihood MI as described in @vanHippelBartlett2021.

A detailed description and comparison of the implemented methods can be found in @Wolbers2021, as well as in the package vignette describing the statistical specifications of the package.

# Acknowledgements

TODO

# References
