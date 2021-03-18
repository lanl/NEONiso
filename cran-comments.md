## Test environments
* local R installation: R 4.0.4
* GitHub Actions (ubuntu-18.04): devel, release, oldrel, 3.5
* GitHub Actions (windows-latest): release, 3.6
* GitHub Actions (macOS-latest): release

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a the a resubmission of a new package.
* Auto-check will issue a warning about misspelled words in DESCRIPTION, but the words flagged are all correct.
* The following changes have been made to address issues raised by Gregor Seyer during review:
  * Removed a space in doi specification in DESCRIPTION to make clickable.
  * Added \value to requested .Rd files.
  