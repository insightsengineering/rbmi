## Summary of Submission

This version of the package fixes the error of the Stan code failing to compile on machines using the C23 standard. The issue was the inclusion of a `#` character within in a comment in the Stan code which the compiler then interpreted as a preprocessor directive. Removing the `#` character resolved this issue. 

Also within this release we have changed the maintainer.

## R CMD check results

```
Status: 1 NOTE

❯ checking CRAN incoming feasibility ... [7s/39s] NOTE
  Maintainer: ‘Isaac Gravestock <isaac.gravestock@roche.com>’
  
  New maintainer:
    Isaac Gravestock <isaac.gravestock@roche.com>
  Old maintainer(s):
    Craig Gower-Page <craig.gower-page@roche.com>
```

## Test environments

The package was tested in the following environments:

- MacOS, R release (Local Machine)
- Windows, R release (Win-Builder)
- MacOS, devel (macOS builder)
- Ubuntu 22.04 LTS, devel (Rhub / GitHub Actions)


## Downstream dependencies

The following reverse dependencies were checked:

- term.rbmi - no issues detected
