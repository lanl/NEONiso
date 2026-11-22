## Test environments
* local R installation (arm64/M4, macOS 27.2.1): R 4.6.2

* GitHub Actions (ubuntu-latest): devel, release, oldrel
* GitHub Actions (windows-latest): release
* GitHub Actions (macOS-latest): release
* win-builder: devel

## Resubmission

* This is a resubmission of a package archived on 2026-03-29 because
check issues were not corrected in time. The issue was a change in the package
providing hdf5 capability. In NEONiso 0.8.0, selection of hdf5r on CRAN was allowed
but one of the imports (neonUtilities) of this package did not yet have this as
an option. As a result, there was an implicit rhdf5 (on Bioconductor) dependency
that we could not resolve until neonUtilities was patched. Upstream has been patched,
along with several bug fixes and performance updates in this new submission of 0.9.0

## R CMD check results

* No errors or warnings in local environment. Note in local environment indicates
that this is a resubmission of an archived package (as described above).

* No errors, warnings, or notes in GitHub Actions.

* No errors, warnings, or notes on win-builder (devel).

* Auto-check will issue a warning about misspelled words in DESCRIPTION, 
but the words flagged are all correct.

# Downstream dependencies

There are currently no downstream dependencies for this package.