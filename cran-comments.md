## Test environments
* local R installation: R 4.2.1
* GitHub Actions (ubuntu-18.04): devel, release, oldrel
* GitHub Actions (windows-latest): release
* GitHub Actions (macOS-latest): devel, release, oldrel
* win-builder: devel, release
* R-hub (Windows Server 2022): devel
* R-hub (ubuntu-20.04.1 LTS): release
* R-hub (fedora): devel

## R CMD check results

* No errors, warnings, or notes in local environment.

* No errors, warnings, or notes in GitHub Actions, except for macOS-latest (devel),
which failed to download R-devel due to a 404 HTTP error

* No errors or warnings on win-builder.

* One error and one note issued on R-hub/Windows Server (devel): error is about a 
bioconductor dependency not being available, note is about change in maintainer
(my email address has changed) and invalid URLs (URLs are correct however). 
Same note is also provided on ubuntu-20.04.1 LTS.

* No errors, warnings, or notes on R-hub fedora.

* Auto-check will issue a warning about misspelled words in DESCRIPTION, but the words flagged are all correct.


# Downstream dependencies

There are currently no downstream dependencies for this package.