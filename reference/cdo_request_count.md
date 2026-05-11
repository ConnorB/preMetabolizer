# Inspect and reset the NCEI CDO session request counter

The NCEI CDO Web Services v2 API enforces a daily limit of 10,000
requests per token. The package counts every successful CDO request made
in the current R session and aborts further requests once the limit is
reached. These helpers expose the counter.

## Usage

``` r
cdo_request_count()

cdo_reset_request_count()
```

## Value

`cdo_request_count()` returns an integer count of successful CDO
requests made this session. `cdo_reset_request_count()` is called for
its side effect of zeroing the counter and returns the prior count
invisibly.

## Details

Because the API does not report remaining quota, the counter is
session-local and best-effort: it does not include requests made by
other R sessions, other tools, or earlier sessions on the same day.

## See also

[`cdo_data()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
and friends.
