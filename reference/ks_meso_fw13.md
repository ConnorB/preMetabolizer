# Get Kansas Mesonet FW13 data

Retrieves fire weather data in FW13 format for one station and date
range.

## Usage

``` r
ks_meso_fw13(station, start_date, end_date)
```

## Arguments

- station:

  Station name as a character string.

- start_date:

  Start date for the data retrieval in `YYYY-MM-DD` or `YYYYMMDD`
  format.

- end_date:

  End date for the data retrieval in `YYYY-MM-DD` or `YYYYMMDD` format.

## Value

A character vector containing FW13 records.

## Details

**\[experimental\]**

Kansas Mesonet data are preliminary and subject to revision. Cite the
Kansas Mesonet when sharing, publishing, or otherwise disseminating data
accessed with this function. A suggested citation format is: Kansas
Mesonet, year: webpage title. Accessed date, webpage URL. Review the
Kansas Mesonet data usage policy before automated use; automated page
scraping or data ingesting without written consent is not permitted.

## References

Kansas Mesonet data usage policy:
<https://mesonet.k-state.edu/about/usage/>

## Examples

``` r
if (FALSE) { # \dontrun{
ks_meso_fw13(
  station = "Konza Prairie",
  start_date = "2024-04-01",
  end_date = "2024-04-07"
)
} # }
```
