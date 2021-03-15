## Test environments
* local R installation: R 4.0.4
* GitHub Actions (ubuntu-18.04): devel, release, oldrel, 3.5
* GitHub Actions (windows-latest): release, 3.6
* GitHub Actions (macOS-latest): release

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a the a resubmission of a new package.
* The following changes have been made to address notes arising during CRAN auto-checks:
  * An invalid URL from inst/doc/example_workflow.html has been corrected.
  * An invalid URI from inst/CITATION has been corrected.
  * The auto-checks indicate that there are misspelled words in DESCRIPTION, but the words flagged are all correct.
