#' Determine the astronomical season from a date
#'
#' Classifies dates into astronomical seasons using the precise dates of
#' equinoxes and solstices for each year, computed from Meeus's
#' "Astronomical Algorithms" (Willmann-Bell, 1991), chapter 26.
#'
#' @param date A `Date`, `POSIXct`, or `POSIXlt` vector, or anything coercible
#'   to `Date` via [base::as.Date()].
#' @param hemisphere One of `"north"` (default) or `"south"`.
#' @param labels Optional named character vector overriding the default
#'   season names. Allowed names are `spring`, `summer`, `autumn`, and
#'   `winter`. Partial overrides are allowed; unspecified names keep their
#'   defaults.
#'
#' @return An ordered factor with levels Spring < Summer < Autumn < Winter
#'   (or the user-supplied equivalents), the same length as `date`. `NA`
#'   in input produces `NA` in output.
#'
#' @details
#' A mean Julian Ephemeris Day for each equinox and solstice is computed
#' from the Table 26.B polynomial and refined by the 24-term periodic
#' correction in Table 26.C. The corrected instant is accurate to roughly
#' a minute and is then rounded to a calendar date in UTC. The formulas
#' are valid for years 1000-3000.
#'
#' Southern-hemisphere seasons are the northern seasons offset by six
#' months. For example, dates after the March equinox and before the June
#' solstice are northern spring but southern autumn.
#'
#' @examples
#' get_season(as.Date(c("2024-03-19", "2024-06-20", "2024-09-22", "2024-12-21")))
#'
#' get_season(Sys.time())
#'
#' # Southern hemisphere
#' get_season(as.Date("2024-07-15"), hemisphere = "south")
#'
#' # Custom labels
#' get_season(as.Date("2024-09-22"), labels = c(autumn = "Fall"))
#'
#' @export
get_season <- function(date, hemisphere = c("north", "south"), labels = NULL) {
  hemisphere <- match.arg(hemisphere)

  date <- tryCatch(base::as.Date(date), error = function(e) {
    cli::cli_abort("{.arg date} must contain valid dates.", parent = e)
  })

  default_labels <- c(
    spring = "Spring",
    summer = "Summer",
    autumn = "Autumn",
    winter = "Winter"
  )

  if (!is.null(labels)) {
    if (
      !is.character(labels) ||
        is.null(names(labels)) ||
        !all(names(labels) %in% names(default_labels))
    ) {
      cli::cli_abort(
        "{.arg labels} must be a named character vector with names from {.val spring}, {.val summer}, {.val autumn}, {.val winter}."
      )
    }
    default_labels[names(labels)] <- labels
  }

  dn <- as.numeric(date)
  if (length(dn)) {
    rng <- range(dn, na.rm = TRUE)
    if (
      is.finite(rng[1]) &&
        (rng[1] < season_range[1L] || rng[2] > season_range[2L])
    ) {
      cli::cli_abort(
        "{.arg date} must contain dates within years 1000 through 3000."
      )
    }
  }

  # findInterval returns 0..length(season_breaks). The interval pattern cycles
  # by year as (winter, spring, summer, autumn, winter, ...). Map bin -> code
  # arithmetically; for south, shift the cycle by two seasons.
  bins <- findInterval(dn, season_breaks)
  shift <- if (hemisphere == "north") 3L else 1L
  codes <- ((bins + shift) %% 4L) + 1L

  structure(
    codes,
    levels = unname(default_labels[c("spring", "summer", "autumn", "winter")]),
    class = c("ordered", "factor")
  )
}

# Meeus (1991) Table 26.C: 24 periodic terms used to refine the mean JDE.
# A is in units of 0.00001 day; B and C are in degrees.
meeus_periodic_terms <- data.frame(
  A = c(
    485,
    203,
    199,
    182,
    156,
    136,
    77,
    74,
    70,
    58,
    52,
    50,
    45,
    44,
    29,
    18,
    17,
    16,
    14,
    12,
    12,
    12,
    9,
    8
  ),
  B = c(
    324.96,
    337.23,
    342.08,
    27.85,
    73.14,
    171.52,
    222.54,
    296.72,
    243.58,
    119.81,
    297.17,
    21.02,
    247.54,
    325.15,
    60.93,
    155.12,
    288.79,
    198.04,
    199.76,
    95.39,
    287.11,
    320.81,
    227.73,
    15.45
  ),
  C = c(
    1934.136,
    32964.467,
    20.186,
    445267.112,
    45036.886,
    22518.443,
    65928.934,
    3034.906,
    9037.513,
    33718.147,
    150.678,
    2281.226,
    29929.562,
    31555.956,
    4443.417,
    67555.328,
    4562.452,
    62894.029,
    31436.921,
    14577.848,
    31931.756,
    34777.259,
    1222.114,
    16859.074
  )
)

# Sorted vector of every equinox and solstice date (as days since 1970-01-01
# UTC) for years 1000-3000, interleaved by year:
#   spring1000, summer1000, autumn1000, winter1000, spring1001, ...
# Built once at package source/install via Meeus (1991) chapter 26: a
# Table 26.B polynomial for the mean JDE per event, refined by the
# 24-term periodic correction in Table 26.C. Vectorized across all 8004
# events at once.
season_breaks <- local({
  years <- 1000:3000
  Y <- (years - 2000) / 1000

  jde_mean <- as.numeric(rbind(
    spring = 2451623.80984 +
      365242.37404 * Y +
      0.05169 * Y^2 -
      0.00411 * Y^3 -
      0.00057 * Y^4,
    summer = 2451716.56767 +
      365241.62603 * Y +
      0.00325 * Y^2 +
      0.00888 * Y^3 -
      0.00030 * Y^4,
    autumn = 2451810.21715 +
      365242.01767 * Y -
      0.11575 * Y^2 +
      0.00337 * Y^3 +
      0.00078 * Y^4,
    winter = 2451900.05952 +
      365242.74049 * Y -
      0.06223 * Y^2 -
      0.00823 * Y^3 +
      0.00032 * Y^4
  ))

  TT <- (jde_mean - 2451545.0) / 36525
  W <- (35999.373 * TT - 2.47) * pi / 180
  delta_lambda <- 1 + 0.0334 * cos(W) + 0.0007 * cos(2 * W)

  tab <- meeus_periodic_terms
  M <- cos(
    (outer(TT, tab$C) +
      matrix(tab$B, nrow = length(TT), ncol = nrow(tab), byrow = TRUE)) *
      pi /
      180
  )
  S <- as.vector(M %*% tab$A)

  jde <- jde_mean + (0.00001 * S) / delta_lambda

  # JD 2440587.5 = 1970-01-01 00:00 UTC. The TT-UTC offset (~70 s in modern
  # years) is negligible relative to the day rounding.
  as.numeric(as.Date(
    as.POSIXct(
      (jde - 2440587.5) * 86400,
      origin = "1970-01-01",
      tz = "UTC"
    ),
    tz = "UTC"
  ))
})

# Inclusive bounds (days since 1970-01-01 UTC) for years 1000-3000.
# The cyclic bin pattern correctly classifies dates before the year-1000
# spring equinox and after the year-3000 winter solstice (both are winter),
# so the valid input range extends beyond the first and last breakpoints.
season_range <- as.numeric(as.Date(c("1000-01-01", "3000-12-31")))
