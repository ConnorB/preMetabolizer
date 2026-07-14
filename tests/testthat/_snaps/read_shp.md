# read_shp warns when a directory contains multiple layers

    Code
      result <- read_shp(dir)
    Condition
      Warning:
      Multiple layers found in '<tempdir>'.
      i Using the first layer: "a".

# read_shp errors when no layers are found

    Code
      read_shp(dir)
    Output
      Cannot open data source <tempdir>
    Condition
      Error:
      ! Open failed.
