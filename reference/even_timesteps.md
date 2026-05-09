# Get evenly spaced time steps for logger data

Creates a dataframe with evenly spaced time steps by merging original
data with a sequence of timestamps at the most common sampling interval.
Can handle multiple sites with different time intervals.

## Usage

``` r
even_timesteps(loggerData, datetime_col = "DateTime_UTC", site_col = NULL)
```

## Arguments

- loggerData:

  A data frame containing a DateTime_UTC column with timestamp data

- datetime_col:

  Character string specifying the datetime column name. Defaults to
  "DateTime_UTC"

- site_col:

  Character string specifying the site column name. If NULL (default),
  treats all data as a single site.

## Value

A data frame or tibble with evenly spaced time steps for each site,
including all original data points plus NAs for missing intervals. The
class matches the input.

## Examples

``` r
if (FALSE) { # \dontrun{
# Single site usage
df <- data.frame(
  DateTime_UTC = seq(as.POSIXct("2024-01-01"), by = "1 hour", length.out = 10)
)
even_timesteps(df)

# Multiple sites
df_multi <- data.frame(
  DateTime_UTC = c(
    seq(as.POSIXct("2024-01-01"), by = "1 hour", length.out = 5),
    seq(as.POSIXct("2024-01-01"), by = "30 min", length.out = 5)
  ),
  Site = c(rep("Site1", 5), rep("Site2", 5))
)
even_timesteps(df_multi, site_col = "Site")
} # }
```
