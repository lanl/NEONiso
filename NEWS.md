# NEONiso 0.5.3

* Small maintenance release that addresses a NOTE on CRAN.

# NEONiso 0.5.2

* Fixes a bug where an rhdf5 function was not being imported properly,
generating warnings and errors on CRAN.
* Updates manage_local_ec_archive to avoid a scenario that was creating
duplicate files, thereby being more efficient with drive space.

# NEONiso 0.5.1

* This update takes advantage of upgrades to the stackEddy function 
in neonUtilities 2.1.1, which allows the calibration routines to run
more efficiently on NEON's HDF5 files. For example, calibrate_carbon(),
runs ~2x faster if neonUtilities 2.1.1 is used instead of earlier versions.

# NEONiso 0.5.0

* Adds the calibrate_carbon() function, which can be used to generate
monthly output files or can be used to bundle all of the months of data
available for an individual site into a single file. In the long-term,
calibrate_carbon() will be the 'workhorse' function for NEON's carbon data,
and calibrate_carbon_bymonth will be deprecated in a future release.

# NEONiso 0.4.0

* This is the initial release on CRAN.
