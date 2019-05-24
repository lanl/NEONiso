# NEONiso
R package for downloading, calibrating, and interacting with NEON atmospheric isotope data

## What functions are complete? (please test!)
1) terrestrial_core_sites - returns a vector of four-letter codes corresponding to NEON core terrstrial sites
2) terrestrial_relocatable_sites - returns a vector of four-letter codes corresponding to NEON relocatable terrestrial sites

## What functions are next?
Algorithms complete, but to be ported over from NEON data processing:
1) carbon isotope calibration functions (Bowling et al. and three point lin-reg)
2) water isotope calibration functions
3) ambient calibration functions
4) function to pluck a variable out at a particular height on a tower.