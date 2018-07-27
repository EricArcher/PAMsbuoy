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
#' @importFrom geosphere bearing
#'
calculateOffset <- function(calibration, position, db) {
  # loop through buoys in calibration list
  for(b in names(calibration)) {
    buoyCal <- calibration[[b]]
    # get buoy position
    if(is.null(buoyCal)) {
      next
    }
    buoyPos <- buoyPosition(buoyCal, position, method='initial')
    # get ship position at calibration point times
    shipPos <- estimatePosition(buoyCal$UTC, db$gpsData)
    # calculate true bearing from buoy to ship for each set of positions
    trueBearing <- if(is.null(buoyPos)) {
      rep(NA, nrow(shipPos))
      # matrix(NA, nrow=2, ncol=nrow(ship.pos))
    } else {
      # mapply(
      # bearing,
      # buoy.pos$Latitude, buoy.pos$Longitude,
      # ship.pos$Latitude, ship.pos$Longitude
      # )
      geosphere::bearing(cbind(buoyPos$Longitude, buoyPos$Latitude),
                         cbind(shipPos$Longitude, shipPos$Latitude))
    }
    # Put in placeholders for calibration data. Useful for these to default to NA
    # for summaries and looking at data
    calibrationValue <- rep(NA, nrow(shipPos))
    calibratedBearing <- rep(NA, nrow(shipPos))
    # add magnetic variation and true bearing to calibration data frame
    buoyCal <- cbind(
      buoyCal,
      magneticVariation = shipPos$MagneticVariation,
      trueBearing = trueBearing %% 360,
      # true.bearing = apply(true.bearing, 2, mean),
      BoatLatitude = shipPos$Latitude,
      BoatLongitude = shipPos$Longitude,
      calibrationValue,
      calibratedBearing
    )
    # calculate offset = true - difar bearing. Make range -180 to 180
    buoyCal$offset <- sapply((buoyCal$trueBearing - buoyCal$DIFARBearing) %% 360, function(x) {
      if(x > 180) {
        x-360
      } else x
    })
    calibration[[b]] <- buoyCal
  }
  calibration
}