# Kansas Mesonet Variables

This dataset contains variable names, units, and descriptions as
provided by the Kansas Mesonet. These variables represent a wide range
of meteorological, hydrological, and soil measurements.

## Usage

``` r
ks_mesonet_vars
```

## Format

A tibble with 150 rows and 4 columns:

- Variable:

  The original variable name as used by the Kansas Mesonet.

- CleanName:

  A cleaned and standardized version of the variable name for easier use
  in R.

- Units:

  The units of measurement for the variable (if applicable).

- Description:

  A brief description of the variable and its purpose.

## Source

Kansas Mesonet, <http://mesonet.k-state.edu/rest/variables/>
