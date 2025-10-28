## Summary of Submission (v1.5.2)

This submissions aims to fix a CRAN check failure (though it appears to have since run fine on a more recent re-run). We weren't able to re-produce or diagnose the exact issue but believe it related to how Stan caches models. As such we have removed all model caching on CRAN and have reduced the number of unit tests run on CRAN to ensure the code runs under 10 minutes. 

## R CMD check results

No notes or warnings.

## Test environments

The package was tested in the following environments:

- MacOS, R release (Local Machine)
- Windows, R release (Win-Builder)
- MacOS, devel (macOS builder)
- Ubuntu 22.04 LTS, devel (GitHub Actions)

## Downstream dependencies

The following reverse dependencies were checked:

- term.rbmi - no issues detected
- junco - no issues detected
- rbmiUtils - no issues detected
