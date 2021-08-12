## Test environments
* local R installation: R 4.1.0
* GitHub Actions (ubuntu-18.04): devel, release, oldrel
* GitHub Actions (windows-latest): release
* GitHub Actions (macOS-latest): devel, release, oldrel
* win-builder: devel
* r-hub: Fedora Linux, R-devel, clang, gfortran
* r-hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC

## R CMD check results
No errors, warnings, or notes on local environment, GitHub Actions, or win-builder.

* Auto-check will issue a warning about misspelled words in DESCRIPTION, but the words flagged are all correct.
* r-hub test environments fail due to issues building Bioconductor packages (rhdf5).
* CRAN web checks also occassionaly issue an error due to issues with rhdf5.

## Downstream dependencies
There are currently no downstream dependencies for this package.
