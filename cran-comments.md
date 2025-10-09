## Summary of Submission (v1.5.1)

This is a re-submission with some tweaks the caching of compiled Stan programs to improve the run time of the test scripts to keep them under CRANs 10-minute limit.

Please note that in this submission we have changed the maintainer back to Craig Gower-Page (the original maintainer) albeit at a new email address.

## Original submission notes (v1.5.0)

This version of the package aims to resolve two issues with CRAN checks.

- ATLAS: A test failed in test-parallel.R. We could not reproduce this error but have identified that an incorrect 
  cluster object was used. This has been corrected.

- donttest: A NOTE due to a .stan file that was left over after testing. This is now cleaned up.

## R CMD check results

No notes or warnings.

## Test environments

The package was tested in the following environments:

- MacOS, R release (Local Machine)
- Windows, R release (Win-Builder)
- MacOS, devel (macOS builder)
- Ubuntu 22.04 LTS, devel (Rhub / GitHub Actions)
- Fedora Linux 38 with ATLAS, devel (Rhub)

## Downstream dependencies

The following reverse dependencies were checked:

- term.rbmi - no issues detected
- junco - no issues detected
- rbmiUtils - no issues detected
