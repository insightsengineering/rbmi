## Summary of Submission

This version of the package removes references to native pipes `|>` and lambda functions `\(x)` to ensure the package is backwards compatible with older versions of R. This update also exposes the control options for `rstan` allowing the user greater control over the MCMC computations.


## R CMD check results

There were no ERRORs, no WARNINGs and no NOTEs.


## Test environments

The package was tested in the following environments:

- MacOS, R release (Local Machine)
- Windows, R release (Win-Builder)
- MacOS, devel (macOS builder)
- Ubuntu 22.04 LTS, devel (Rhub / GitHub Actions)


## Downstream dependencies

The following revdeps were checked:

- term.rbmi - no issues detected
