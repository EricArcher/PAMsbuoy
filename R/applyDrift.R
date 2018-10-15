#' @name applyDrift
#' @title Apply Drift Calibration
#' @description Applies estimated drift to detection and calibration data. Detection
#'   and calibration data frames will have two new columns \code{driftedBuoyLat}
#'   and \code{driftedBuoyLong}. These are calculated based on the drift rate, bearing,
#'   and time since deployment.
#'
#' @param stationList a list of sonobuoy stations as created by \code{loadStations},
#'   or a single station as created by \code{formatStation}
#' @param myStations IDs of stations to check. Needed for calibrateStations to work correctly
#' @param recalibrate should buoys that have already been checked be re-examined? If
#'   \code{FALSE}, any buoys with existing drift data will be skipped over.
#' @param \dots arguments passed on to other functions
#'
#' @importFrom dplyr select
#' @importFrom geosphere destPoint bearing
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
applyDrift <- function(stationList, myStations, recalibrate = FALSE, ...) {
  calibrateCount <- 0
  skipCount <- 0

  # Select what calibration qualities are okay to use
  qualityOptions <- c('Good', 'Questionable', 'Bad')
  qualityCheck <- qualityOptions[
    1:menu(title='What quality buoys should we apply drift to?',
           choices = c('Good only', 'Good and Questionable', 'Good, Questionable, and Bad'))]
  for(s in myStations) {
    if(is.null(attr(stationList[[s]], 'station'))) {
      warning('Object ', s, ' is not a station. Please check input.')
      next
    } else if(!('buoys' %in% names(stationList[[s]]))) {
      warning('Station ', s, ' does not have any buoys.')
      next
    }
    for(b in seq_along(stationList[[s]]$buoys)) {
      thisBuoy <- stationList[[s]]$buoys[[b]]$calibration$Buoy[1]
      whichThisBuoy <- stationList[[s]]$detections$Buoy==thisBuoy
      thisBuoyData <- stationList[[s]]$buoys[[b]]

      # If columns exist we have already calibrated, so don't recal
      if(!recalibrate && ('driftedBuoyLat' %in% colnames(thisBuoyData$calibration))) {
        next
      }

      if(is.na(thisBuoyData$info$drift$rate)) {
        skipCount <- skipCount + 1
        next
      }
      # if(!(thisBuoyData$info$drift$quality %in% qualityCheck)) {
      #   next
      # }
      if(!(thisBuoyData$info$buoyQuality %in% qualityCheck)) {
        next
      }

      driftCals <- driftBuoy(position = thisBuoyData$position,
                             times = thisBuoyData$calibration$UTC,
                             drift = thisBuoyData$info$drift)
      driftBearing <- geosphere::bearing(
        cbind(driftCals$driftedBuoyLong, driftCals$driftedBuoyLat),
        cbind(thisBuoyData$calibration$BoatLongitude, thisBuoyData$calibration$BoatLatitude)) %% 360

      driftOffset <- (driftBearing - thisBuoyData$calibration$DIFARBearing) %% 360
      driftOffset <- ifelse(driftOffset > 180, driftOffset - 360, driftOffset)

      driftDets <- driftBuoy(position = thisBuoyData$position,
                             times = stationList[[s]]$detections$UTC[whichThisBuoy],
                             drift = thisBuoyData$info$drift)
      # Init with NA since we only fill part of DF at a time
      if(!('driftedBuoyLat' %in% colnames(stationList[[s]]$detections))) {
        nDet <- nrow(stationList[[s]]$detections)
        stationList[[s]]$detections$driftedBuoyLat <- rep(NA, nDet)
        stationList[[s]]$detections$driftedBuoyLong <- rep(NA, nDet)
      }
      stationList[[s]]$detections[whichThisBuoy, 'driftedBuoyLat'] <- driftDets$driftedBuoyLat
      stationList[[s]]$detections[whichThisBuoy, 'driftedBuoyLong'] <- driftDets$driftedBuoyLong

      stationList[[s]]$buoys[[thisBuoy]]$calibration$driftedBuoyLat <- driftCals$driftedBuoyLat
      stationList[[s]]$buoys[[thisBuoy]]$calibration$driftedBuoyLong <- driftCals$driftedBuoyLong
      stationList[[s]]$buoys[[thisBuoy]]$calibration$driftedTrueBearing <- driftBearing
      stationList[[s]]$buoys[[thisBuoy]]$calibration$driftedOffset <- driftOffset
      calibrateCount <- calibrateCount + 1
    }
  }
  if(skipCount > 0) {
    cat('Skipped', skipCount, 'buoys with no drift calibration data. \n')
  }
  cat('Applied drift calibration to', calibrateCount, 'buoys total.')
  stationList
}

driftBuoy <- function(position, times, drift) {
  if(length(times)==0) {
    return(data.frame(driftedBuoyLat=numeric(0),
                      driftedBuoyLong=numeric(0)))
  }
  position <- position[1,]
  df <- data.frame(destPoint(c(position$Longitude, position$Latitude), drift$bearing,
            difftime(times, position$UTC, units='secs') * drift$rate * 1000 / 3600))
  colnames(df) <- c('driftedBuoyLong', 'driftedBuoyLat')
  select(df, driftedBuoyLat, driftedBuoyLong)
}
