#' Converts stream discharge in cubic feet per second to cubic meters per second
#'
#' @param Qcfs a numeric vector of stream discharge in cubic feet per second
#'
#' @return a numeric vector of stream discharge in cubic meters per second
#' @export
#'
#' @examples
#' Q <- seq(1,10,2)
#' cfs_to_cms(Q)
cfs_to_cms <- function(Qcfs){
  Qcfs * 0.028316847 #Convert cfs to cms
}
