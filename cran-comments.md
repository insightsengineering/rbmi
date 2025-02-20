## Summary of Submission

This version of the package fixes the error of the Stan code failing to compile on machines using the C23 standard. The issue was the inclusion of a `#` character within in a comment which the compiler interprets as a preprocessor directive. Removing the `#` character resolved this issue. 

Also within this release we have changed the maintainer.

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
