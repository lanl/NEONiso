## Test environments
* local R installation (x86_64, macOS 13.5.2): R 4.3.1
* local R installation (arm64/M1 Max, macOS 13.5.2): R 4.3.1
* GitHub Actions (ubuntu-latest): devel, release, oldrel
* GitHub Actions (windows-latest): release
* GitHub Actions (macOS-latest): release
* win-builder: devel, release, oldrel
* R-hub (Windows Server 2022): devel
* R-hub (ubuntu-20.04.1 LTS): release
* R-hub (fedora): devel

## R CMD check results

* No errors, warnings, or notes in local environment.

* No errors, warnings, or notes in GitHub Actions.

* No errors, warnings, or notes on win-builder (devel, release). Win-builder oldrel
provides a warning about a possibly invalid URL, but URLs are correct and are 
just slow to redirect.

* R-hub / Windows Server 2022 / R-devel issues 1 error about a bioconductor
dependency not being available (rhdf5), presumably due to a mismatch between R-devel
and the bioconductor release schedule. R-hub / ubuntu-20.04.1 LTS / R-release
throws a note about HTML tidy not being available, which appears to be an issue
with the testing platform and not the package. R-hub / fedora / R-devel indicates
PREPERROR, but build and install logs look fine.

* Auto-check will issue a warning about misspelled words in DESCRIPTION, 
but the words flagged are all correct.

# Downstream dependencies

There are currently no downstream dependencies for this package.