mesonet_clean_name <- function(x) {
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x, perl = TRUE)
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x, perl = TRUE)
  x <- gsub("([0-9])([A-Za-z])", "\\1_\\2", x, perl = TRUE)
  x <- gsub("[^A-Za-z0-9]+", "_", x, perl = TRUE)
  x <- tolower(x)
  x <- gsub("^_+|_+$", "", x, perl = TRUE)
  gsub("_+", "_", x, perl = TRUE)
}

mesonet_rename_columns <- function(data, replacements = character()) {
  old_names <- names(data)
  new_names <- mesonet_clean_name(old_names)

  if (length(replacements) > 0) {
    matched <- match(names(replacements), old_names)
    matched <- matched[!is.na(matched)]
    new_names[matched] <- unname(
      replacements[match(old_names[matched], names(replacements))]
    )
  }

  names(data) <- new_names
  tibble::as_tibble(data)
}

mesonet_clean_named <- function(x) {
  if (!is.null(names(x))) {
    names(x) <- mesonet_clean_name(names(x))
  }

  x
}

mesonet_parse_utc_datetime <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }

  parsed <- lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE)
  missing <- is.na(parsed) & !is.na(x)
  parsed[missing] <- lubridate::ymd_hm(x[missing], tz = "UTC", quiet = TRUE)
  parsed
}
