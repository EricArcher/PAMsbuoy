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
#' @importFrom swfscMisc bearing
#'
calculateOffset <- function(calibration, position, db) {
  # loop through buoys in calibration list
  for(b in names(calibration)) {
    b.cal <- calibration[[b]]
    # get buoy position
    if(is.null(b.cal)) {
      next
    }
    buoy.pos <- buoyPosition(b.cal, position, method='initial')
    # get ship position at calibration point times
    ship.pos <- estimatePosition(b.cal$UTC, db$gpsData)
    # calculate true bearing from buoy to ship for each set of positions
    true.bearing <- if(is.null(buoy.pos)) {
      matrix(NA, nrow=2, ncol=nrow(ship.pos))
    } else mapply(
      bearing,
      buoy.pos$Latitude, buoy.pos$Longitude,
      ship.pos$Latitude, ship.pos$Longitude
    )
    # add magnetic variation and true bearing to calibration data frame
    b.cal <- cbind(
      b.cal,
      magnetic.variation = ship.pos$MagneticVariation,
      true.bearing = apply(true.bearing, 2, mean),
      BoatLatitude = ship.pos$Latitude,
      BoatLongitude = ship.pos$Longitude
    )
    # calculate offset = true - difar bearing
    b.cal$offset <- b.cal$true.bearing - b.cal$DIFARBearing
    calibration[[b]] <- b.cal
  }
  calibration
}