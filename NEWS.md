# NEONiso 0.6.1.9000

* Exports new helper function for getting sites with water isotopes, 
water_isotope_sites().
* The reference_corrections vignette was blank in the previous release - 
it is updated in this release (#81)
* Makes select functions used internally consistent with upcoming changes
to tidyselect (h/t Hadley Wickham)

# NEONiso 0.6.0

* An experimental calibration routine for water isotopes has been added. It does
have some known issues (e.g., no correction is made for concentration dependence
of the analyzers yet), and any data produced from this function should be considered
provisional.
* Added capability to plot data used in carbon calibration regression in order
to help identify periods where calibration parameters seem to be okay, but
quality of calibrated data is degraded.
* Added cross-validation error estimates to carbon calibration routines.
* The calibrate_carbon_bymonth() function has been marked as deprecated, but will
be removed no earlier than version 0.7.0 or the end of 2022. 
The more flexible calibrate_carbon() function should be used.
* A bug that generated non-nonsensical CO2 and d13C values has been fixed (#72).
* The calibrate_carbon() function now also provides calibrated values for
reference material measurement. These are useful for determining calibration
error when one of the reference materials is omitted from the calibration (e.g., 
generate calibration relationships to the high and medium standards, then estimate
error as the difference between calibrated measurement and known reference values).
* Reduced the number of tests that run on CRAN to minimize compute resources requested.
* Made a function that corrects a few mismatched reference values more visible,
and added a few more instances where reference data needs to be corrected.

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
