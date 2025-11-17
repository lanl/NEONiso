# Manage a local eddy covariance (EC) data archive.

Utility function to help retrieve new EC data and/or prune duplicates,
as NEON provisions new data or re-provisions data for an existing site
and month.

## Usage

``` r
manage_local_EC_archive(
  file_dir,
  get = TRUE,
  unzip_files = TRUE,
  trim = FALSE,
  dry_run = TRUE,
  sites = "all",
  release = "RELEASE-2024"
)
```

## Arguments

- file_dir:

  Specify the root directory where the local EC store is kept.

- get:

  Pull down data from NEON API that does not exist locally?

- unzip_files:

  NEON gzips the hdf5 files, should we unzip any gzipped files within
  file_dir? (Searches recursively)

- trim:

  Search through local holdings, and remove older file where there are
  duplicates?

- dry_run:

  List files identified as duplicates, but do not actually delete them?
  Default true to prevent unintended data loss.

- sites:

  Which sites to retrieve data from? Default will be all sites with
  available data, but can specify a single site or a vector here.

- release:

  Download data corresponding to a specific release? Defaults to
  "RELEASE-2024." To download all data, including provisional data, set
  to NULL.

## Value

Returns nothing to the environment, but will download new NEON HDF5
files for selected sites (if `get = TRUE`), unzip them in the local file
directory (if `unzip_files = TRUE`), and identify and remove suspected
duplicate files (if `trim = TRUE` and `dry_run = FALSE`).

## Author

Rich Fiorella <rfiorella@lanl.gov>
