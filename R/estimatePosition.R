#' @title Estimate Position
#' @description Estimate positions for a set of times using a data frame of
#'   time and position combinations
#'
#' @param x vector of times in \code{POSIXct} format
#' @param pos a data.frame with time in \code{POSIXct} in a column labelled
#'   \code{UTC} and position in columns named \code{Latitude} and \code{Longitude}
#'
#' @return a data frame of estimated latitude and longitude for each element in
#'   \code{time} and magnetic variation if a column called \code{MagneticVariation}
#'   exists in \code{pos}
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom stats approx
#' @export
#'
estimatePosition <- function(x, pos) {
  tryCatch({df <- data.frame(
    Latitude = approx(pos$UTC, pos$Latitude, x, ties=mean)$y,
    Longitude = approx(pos$UTC, pos$Longitude, x, ties=mean)$y
  )},
  warning = function(w) {
    browser()
  })
  if("MagneticVariation" %in% colnames(pos)) {
    df$MagneticVariation <- approx(pos$UTC, pos$MagneticVariation, x, ties=mean)$y
  }
  df
}