#' Convert Barometric Pressure
#' @description convert barometric pressure from kilopascals to millibar corrected for elevation

#' @param bp baro pressure in kilopascals
#' @param temp temperature in celcius
#' @param elevation elevation in meters
#'
#' @return elevation corrected pressure
#' @export
#'
#' @examples
seaLev_to_stationBP <- function(bp, temp, elevation){
  mp <- bp *10 #Convert kilopascals to millibar
  K <- temp + 273.15 #Convert celcius to kelvin
  corPres <- mp * exp(-elevation/(K*29.263)) #formula from https://www.sandhurstweather.org.uk/barometric.pdf
  return(corPres)
}
