#' @title Calculate Bearing Offset
#' @description Calculate offset between DIFAR bearing and true bearing for
#'   calibration
#'
#' @param calibration calibration list of buoys for a station
#' @param position position list of buoys for a station
#' @param db database list of data frames for a station
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom stats approx
#' @export
#'
calculateOffset <- function(calibration, position, db) {
  for(b in names(calibration)) {
    b.cal <- calibration[[b]]
    buoy.pos <- buoyPosition(b.cal, position)
    ship.pos <- estimatePosition(b.cal$UTC, db$gpsData)
    true.bearing <- mapply(
      bearing,
      buoy.pos$Latitude, buoy.pos$Longitude,
      ship.pos$Latitude, ship.pos$Longitude
    )
    b.cal <- cbind(
      b.cal,
      magnetic.variation = ship.pos$MagneticVariation,
      true.bearing = apply(true.bearing, 2, mean)
    )
    b.cal$offset <- b.cal$true.bearing - b.cal$DIFARBearing
    calibration[[b]] <- b.cal
  }
  calibration
}