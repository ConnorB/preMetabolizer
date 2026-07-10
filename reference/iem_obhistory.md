# Get one day of Iowa Environmental Mesonet observations (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`iem_ob_history()`](https://connorb.github.io/preMetabolizer/reference/iem_ob_history.md)
instead.

## Usage

``` r
iem_obhistory(station, network, date = Sys.Date(), full = FALSE)
```

## Examples

``` r
# Old:
# iem_obhistory("DSM", network = "IA_ASOS", date = "2024-06-01")
# New:
if (FALSE) { # \dontrun{
iem_ob_history("DSM", network = "IA_ASOS", date = "2024-06-01")
} # }
```
