# Reference Corrections

date: 2022-09-21

There are several instances across the atmospheric isotope measurements
made by NEON where there appears to be a disconnect between the “known”
isotope ratio (and in the case of CO2, the mixing ratio) of the
reference materials used. NEONiso attempts to correct these to maximize
data coverage, primarily through: a) analysis of time series plots of
reference and measured values of validation gases, and/or b)
consultation with NEON staff.

As of version 0.6.0, these corrections are only applied to carbon
isotopes, as the water isotope calibration functions are still
experimental and have known deficiencies.

The list of carbon isotope reference material corrections can be
accessed using the ‘carb’ data object bundled with NEONiso:

    #> # A tibble: 27 × 11
    #>    site  refGas  startDate           co2_old co2_corr co2_repairedRaw d13C_old
    #>    <chr> <chr>   <dttm>                <dbl>    <dbl> <lgl>              <dbl>
    #>  1 ABBY  co2High 2019-10-03 00:00:00    509.      NA  FALSE              -9.65
    #>  2 CLBJ  co2High 2020-01-09 00:00:00    508.      NA  FALSE              -9.26
    #>  3 GRSM  co2High 2020-03-11 00:00:00    533.      NA  FALSE             -13.0 
    #>  4 JERC  co2High 2019-11-13 00:00:00    525.      NA  FALSE             -10.4 
    #>  5 JERC  co2High 2020-02-25 00:00:00    507.      NA  FALSE              -9.53
    #>  6 MOAB  co2Low  2020-03-11 00:00:00    368.      NA  FALSE             -12.4 
    #>  7 MOAB  co2Med  2020-03-11 00:00:00    444.      NA  FALSE              -9.06
    #>  8 BARR  co2High 2018-06-01 00:00:00    425.     555. FALSE              -9.99
    #>  9 BARR  co2Low  2018-06-01 00:00:00    386.     359. FALSE              -8.83
    #> 10 BLAN  co2Low  2019-04-01 00:00:00    361.     363. FALSE              -8.54
    #> 11 BLAN  co2Med  2019-04-01 00:00:00    414.     436. FALSE              -9.52
    #> 12 BLAN  co2High 2019-04-01 00:00:00    509.     521. FALSE             -10.9 
    #> 13 ONAQ  co2Low  2018-06-18 00:00:00    421.     367. FALSE              -9.38
    #> 14 ONAQ  co2Med  2018-06-18 00:00:00    451.     412. FALSE             -10.6 
    #> 15 ONAQ  co2High 2018-06-18 00:00:00    472.     467. FALSE             -13.0 
    #> 16 ORNL  co2Low  2020-01-01 00:00:00    351      368. TRUE               -8.6 
    #> 17 ORNL  co2Med  2019-11-14 00:00:00    530.     443. FALSE             -15.9 
    #> 18 ORNL  co2High 2019-11-14 00:00:00    443.     530. FALSE             -10.6 
    #> 19 SRER  co2Low  2018-04-01 00:00:00    385      356. TRUE               -9.15
    #> 20 SRER  co2Med  2018-04-01 00:00:00    490      429. TRUE              -14.1 
    #> 21 STER  co2Low  2019-01-16 00:00:00    351      360. FALSE              -8.5 
    #> 22 TREE  co2Low  2020-01-01 00:00:00    345      369. TRUE               -8.9 
    #> 23 TREE  co2Med  2020-01-01 00:00:00    395      438. TRUE               -9.9 
    #> 24 TREE  co2High 2020-01-01 00:00:00    525      511. TRUE              -16.1 
    #> 25 WOOD  co2Low  2018-06-18 00:00:00    363.     367. FALSE              -8.83
    #> 26 WOOD  co2Med  2018-06-18 00:00:00    460.     413. FALSE             -10.5 
    #> 27 WOOD  co2High 2018-06-18 00:00:00    564.     455. FALSE             -11.6 
    #>    d13C_corr d13C_repairedRaw versionAdded
    #>        <dbl> <lgl>                   <dbl>
    #>  1     NA    FALSE                     0.6
    #>  2     NA    FALSE                     0.6
    #>  3     NA    FALSE                     0.6
    #>  4     NA    FALSE                     0.6
    #>  5     NA    FALSE                     0.6
    #>  6     NA    FALSE                     0.6
    #>  7     NA    FALSE                     0.6
    #>  8    -11.5  FALSE                     0.4
    #>  9     -8.61 FALSE                     0.4
    #> 10     -8.96 FALSE                     0.4
    #> 11    -10.1  FALSE                     0.4
    #> 12    -15.4  FALSE                     0.4
    #> 13     -9.05 FALSE                     0.4
    #> 14     -9.10 FALSE                     0.4
    #> 15    -10.5  FALSE                     0.4
    #> 16     -9.05 TRUE                      0.4
    #> 17    -10.6  FALSE                     0.6
    #> 18    -15.6  FALSE                     0.6
    #> 19     -8.70 TRUE                      0.4
    #> 20    -10.4  TRUE                      0.4
    #> 21     -8.76 TRUE                      0.4
    #> 22     -9.18 TRUE                      0.4
    #> 23    -10.3  TRUE                      0.4
    #> 24    -14.8  TRUE                      0.4
    #> 25     -9.27 FALSE                     0.4
    #> 26     -9.45 FALSE                     0.4
    #> 27    -10.4  FALSE                     0.4
    #>    notes                                                              
    #>    <chr>                                                              
    #>  1 NA                                                                 
    #>  2 NA                                                                 
    #>  3 NA                                                                 
    #>  4 NA                                                                 
    #>  5 NA                                                                 
    #>  6 NA                                                                 
    #>  7 NA                                                                 
    #>  8 NA                                                                 
    #>  9 NA                                                                 
    #> 10 NA                                                                 
    #> 11 NA                                                                 
    #> 12 NA                                                                 
    #> 13 NA                                                                 
    #> 14 NA                                                                 
    #> 15 NA                                                                 
    #> 16 NA                                                                 
    #> 17 issue first corrected in 0.4, updated for 0.6 - still looks suspect
    #> 18 issue first corrected in 0.4, updated for 0.6                      
    #> 19 NA                                                                 
    #> 20 NA                                                                 
    #> 21 NA                                                                 
    #> 22 NA                                                                 
    #> 23 NA                                                                 
    #> 24 NA                                                                 
    #> 25 NA                                                                 
    #> 26 NA                                                                 
    #> 27 NA

This table lists, in order, a) the site affected; b) the reference gas
affected; c) the start and end dates to which the correction is applied;
d) the suspect CO2 reference mole fraction; e) the corrected CO2 mole
fraction; f) logical of whether files recently pulled from the NEON data
portal are expected to have the CO2 values already corrected; f) the
suspect d13C value; g) the corrected d13C value; h) the version of
NEONiso where that entry was added to the table; and i) any additional
notes regarding the correction.

Note that because this table may change over time, it is possible that
calibrations performed by different versions of the code may change over
time (though, hopefully for the better.) Please post any suspected
missing values to the maintainer as issues on the GitHub repo.
