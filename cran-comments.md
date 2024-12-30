## Test environments
* local R installation (arm64/M2, macOS 15.2): R 4.4.2
* GitHub Actions (ubuntu-latest): devel, release, oldrel
* GitHub Actions (windows-latest): release
* GitHub Actions (macOS-latest): release
* win-builder: devel

## R CMD check results

* No errors, warnings, or notes in local environment. Building manual with
--as-cran issues a note, but seems to be a bunch of HTML warnings that do not
actually prevent building of the manual.

* No errors, warnings, or notes in GitHub Actions.

* No errors, warnings, or notes on win-builder (devel).

* Auto-check will issue a warning about misspelled words in DESCRIPTION, 
but the words flagged are all correct.

# Downstream dependencies

There are currently no downstream dependencies for this package.