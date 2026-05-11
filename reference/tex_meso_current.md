# Get current TexMesonet data

Retrieves the most recent observation from each TWDB TexMesonet station.

## Usage

``` r
tex_meso_current()
```

## Value

A tibble containing the most recent station observations, including
`station_id`, `station_name`, and UTC `recorded_time`. The API returns
availability by station, so some measurement columns may contain missing
values. The returned tibble has a `"units"` attribute containing units
reported by the API.

## Details

**\[experimental\]**

Current data are near-real-time observations. TexMesonet reports values
in the units returned by the API, available with `attr(x, "units")`.

## References

TexMesonet APIs: <https://www.texmesonet.org/Apis>

## Examples

``` r
if (FALSE) { # \dontrun{
current <- tex_meso_current()
attr(current, "units")
} # }
```
