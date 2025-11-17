# Calculate 12C-CO2 Mole Fractions

This function calculates mole fractions of 12CO2 based on the total CO2
mole fraction, the delta13C value of the mixture, and the assumed
fraction of CO2 that does not correspond to 12CO2 or 13CO2 (assumed
fixed at 0.00474, e.g., Griffis et al. 2004 Agricultural and Forest
Meteorology)

## Usage

``` r
calculate_12CO2(total_co2, delta13c, f = 0.00474)
```

## Arguments

- total_co2:

  Vector of CO2 mole fractions.

- delta13c:

  Vector of d13C values.

- f:

  Fraction of CO2 that is not 12CO2 or 13CO2. Assumed fixed at 0.00474

## Value

Vector of 12CO2 mole fractions.

## Author

Rich Fiorella <rfiorella@lanl.gov>

## Examples

``` r
calculate_12CO2(total_co2 = 410, delta13c = -8.5)
#> [1] 403.583
```
