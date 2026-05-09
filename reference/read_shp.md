# Read Shapefile or Zipped Shapefile

Reads a shapefile or shapefile contained in a zip archive, automatically
handling path specifications and layer detection. This is a convenience
wrapper around
[`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html)
that simplifies reading common shapefile formats.

## Usage

``` r
read_shp(path, layer = NULL)
```

## Arguments

- path:

  Path to the shapefile (.shp), directory containing shapefile
  components, or zip file containing a shapefile. Tilde expansion is
  performed.

- layer:

  Layer name to read (optional). If not specified and path points to a
  .shp file, the layer name will be derived from the filename. For
  directories or zip files with multiple layers, the first layer will be
  used with a warning.

## Value

An sf object containing the spatial data from the shapefile.

## Details

The function handles several input types:

- Direct paths to .shp files (automatically extracts directory and layer
  name)

- Directories containing shapefile components (auto-detects layer)

- Zip files containing shapefiles (uses GDAL's virtual file system)

When reading from zip files, the function uses GDAL's `/vsizip/` virtual
file system prefix.

## See also

[`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html)
for the underlying reading function,
[`sf::st_layers()`](https://r-spatial.github.io/sf/reference/st_layers.html)
for layer detection

## Examples

``` r
if (FALSE) { # \dontrun{
# Read from .shp file path
shp_data <- read_shp("path/to/file.shp")

# Read from directory containing shapefile components
shp_data <- read_shp("path/to/shapefile_directory")

# Read from zip file
shp_data <- read_shp("path/to/archive.zip")

# Specify layer explicitly
shp_data <- read_shp("path/to/multi_layer.zip", layer = "specific_layer")
} # }
```
