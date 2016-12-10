#' @title Clip data.frame for Buoy Location
#' @description Remove buoys outside of requested range.
#'
#' @param df data.frame of DIFAR data
#' @param lat.range latitude range to include
#' @param lon.range longitude range to include
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom swfscMisc isBetween
#' @export
#'
clipBuoyLatLon <- function(df, lat.range = NULL, lon.range = NULL) {
  if(is.null(lat.range)) lat.range <- range(df$BuoyLatitude)
  if(is.null(lon.range)) lon.range <- range(df$BuoyLongitude)
  df <- df[!(is.na(df$BuoyLatitude) | is.na(df$BuoyLongitude)), ]
  lat.good <- isBetween(df$BuoyLatitude, lat.range, include.ends = TRUE)
  lon.good <- isBetween(df$BuoyLongitude, lon.range, include.ends = TRUE)
  df[lat.good & lon.good, ]
}
