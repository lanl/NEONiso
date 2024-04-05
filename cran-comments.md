## Test environments
* local R installation (x86_64, macOS 14.4.1): R 4.3.3
* local R installation (arm64/M1 Max, macOS 14.4.1): R 4.3.3
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

* R-hub / Windows Server 2022 / R-devel issues 3 notes about some missing LaTeX 
packages. R-hub / ubuntu-20.04.1 LTS / R-release and R-hub / fedora / R-devel
throws a note about HTML tidy not being available, which appears to be an issue
with the testing platform and not the package.

* Auto-check will issue a warning about misspelled words in DESCRIPTION, 
but the words flagged are all correct.

# Downstream dependencies

There are currently no downstream dependencies for this package.