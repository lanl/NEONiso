## Test environments
* local R installation: R 4.2.1
* GitHub Actions (ubuntu-18.04): devel, release, oldrel
* GitHub Actions (windows-latest): release
* GitHub Actions (macOS-latest): devel, release
* win-builder: devel, release, oldrel
* R-hub (Windows Server 2022): devel
* R-hub (ubuntu-20.04.1 LTS): release
* R-hub (fedora): devel

## R CMD check results

* No errors, warnings, or notes in local environment.

* No errors, warnings, or notes in GitHub Actions.

* No errors, warnings, or on win-builder (devel). Win-builder release and oldrel
provide a warning about a possibly invalid URL, but URLs are correct and are 
just slow to redirect.

* One error and one note issued on all R-hub setups: error is about a 
bioconductor dependency not being available, note is about change in maintainer
(my email address has changed) and invalid URLs (URLs are correct).

* Auto-check will issue a warning about misspelled words in DESCRIPTION, 
but the words flagged are all correct.

# Downstream dependencies

There are currently no downstream dependencies for this package.