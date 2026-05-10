# preMetabolizer: Prepare data for stream metabolism modeling

preMetabolizer provides helpers for building the environmental time
series commonly needed before fitting stream metabolism models. It
includes tools to download external meteorological and elevation data,
align irregular time series, convert units, calculate water and gas
chemistry quantities, and assemble light and pressure inputs for
streamMetabolizer.

## Coordinates

Functions that require locations use `latitude` and `longitude` in
decimal degrees. Latitude ranges from -90 to 90, longitude ranges from
-180 to 180, and western longitudes are negative.

## Main workflows

- Download site-level environmental data with
  [`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md),
  [`get_usgs_elev()`](https://connorb.github.io/preMetabolizer/reference/get_usgs_elev.md),
  [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md),
  and
  [`get_ks_meso()`](https://connorb.github.io/preMetabolizer/reference/get_ks_meso.md).

- Prepare regularly spaced time series with
  [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md)
  and
  [`get_season()`](https://connorb.github.io/preMetabolizer/reference/get_season.md).

- Calculate light, barometric pressure, dissolved oxygen saturation,
  water density, stream depth, and dissolved carbon dioxide inputs.

- Convert common hydrology and atmospheric units with
  [`convert_flow()`](https://connorb.github.io/preMetabolizer/reference/convert_flow.md)
  and
  [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md).

## See also

[`streamMetabolizer::metab()`](https://rdrr.io/pkg/streamMetabolizer/man/metab.html),
<https://connorb.github.io/preMetabolizer/>, and
<https://github.com/ConnorB/preMetabolizer/issues>.

## Author

**Maintainer**: Connor Brown <ConnorBrown1996@gmail.com>
([ORCID](https://orcid.org/0000-0002-9680-8930))

Authors:

- Connor Brown <ConnorBrown1996@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-9680-8930))
