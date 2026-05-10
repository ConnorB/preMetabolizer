# Get the last modified time of a remote file

This internal function retrieves the "Last-Modified" timestamp of a
remote file by sending an HTTP HEAD request to the given URL. The result
is memoised to cache repeated calls with the same URL.

## Usage

``` r
get_remote_mtime(url)
```

## Arguments

- url:

  A character string specifying the URL of the remote file.

## Value

A POSIXlt object representing the last modified timestamp of the remote
file, or `NULL` if the information is unavailable or an error occurs.

## Details

The function sends an HTTP HEAD request to the specified URL using the
httr2 package. If the server responds with a 200 status code and
includes a "Last-Modified" header, the timestamp is parsed and returned.
If the request fails or the "Last-Modified" header is missing, `NULL` is
returned.

## Note

The function uses memoise to cache results, so repeated calls with the
same URL will not trigger additional HTTP requests.
