## Summary of Submission (v1.6.1)

rbmi 1.6.0 currently has a NOTE on CRAN / R-devel regarding calls to `structure()` using deprecated 
special names in file `rbmi/tests/testthat/test-impute.R`, where '.Dim' should be changed to 'dim'.

Release 1.6.1 is a bug-fix release that fixes this NOTE and
* adds a missing vignette on retrieved-dropout models
* improves documentation (return values and example code)
* improves accuracy of several error messages
* implements comprehensive spell-checking against en-GB
* fixes many spelling issues
* updates the e-mail address of one of the authors (Marcel Wolbers) to the current one

## R CMD check results

Status: OK

## Reverse dependency check results

✔ junco 0.1.6                            ── E: 0     | W: 0     | N: 0 
✔ rbmiUtils 0.3.0                        ── E: 0     | W: 0     | N: 0 
OK: 2    

## Test environments

The package was tested in the following environments:

- macOS, R oldrel (Local Machine)
- Ubuntu 24.04.4 LTS, R devel (rhub)
- Windows, R devel (Windows Builder)
