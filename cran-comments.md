## Test environments
* local R installation: R 4.1.2
* GitHub Actions (ubuntu-18.04): devel, release, oldrel
* GitHub Actions (windows-latest): release
* GitHub Actions (macOS-latest): devel, release, oldrel
* win-builder: devel

## R CMD check results

* No errors, warnings, or notes in local environment.
* No errors, warnings, or notes in GitHub Actions, except for windows-latest where
R CMD check does not run due to an error in 'session info' 
(Error: invalid version specification '<NA'>). This error does not seem to be related
to the package itself.
* No errors or warnings on win-builder. Fixed a URL as requested by Uwe Ligges.
Note generated now that this is a "new submission," as package was archived earlier
today. I just moved however, and wasn't able to get the new package accepted by CRAN
within the two weeks. 


* Auto-check will issue a warning about misspelled words in DESCRIPTION, but the words flagged are all correct.


# Downstream dependencies

There are currently no downstream dependencies for this package.