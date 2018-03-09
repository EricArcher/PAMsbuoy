#' @name applyCalibration
#' @title Apply Bearing Calibration
#' @description Return calibrated DIFAR angles using different methods
#'
#' @param angle.data data.frame containing a \code{Buoy} column and a
#' \code{DIFARBearing} column with uncalibrated angle data
#' @param buoy.data list of data.frames containing buoy calibration data
#' @param method method of angle calibration to use. \code{magnetic} uses
#' the median of magnetic varation values. \code{median} uses the median
#' of offset angles. \code{sine} uses sinusoidal fit to error.
#' @param \dots arguments passed on to other functions
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
applyCalibration <- function(stationList, myStations, recalibrate = FALSE, method = '', ...) {
  calibrateCount <- 0
  skipCount <- 0
  calibrationMethods <- c('mean', 'median', 'magnetic')
  # If supplied, assume using same for all. Else ask.
  if(!(method %in% calibrationMethods)) {
    useSame <- ifelse(menu(title='Should the same calibration method be used for all buoys?',
                           choices=c('Yes', 'No'))==1, TRUE, FALSE)
    if(useSame) {
      method <- calibrationMethods[menu(title='What calibration method should be used?',
                                        choices=c('Mean', 'Median', 'Magnetic'))]
    }
  } else {
    useSame <- TRUE
  }
  # Select what calibration qualities are okay to use
  qualityOptions <- c('Good', 'Questionable', 'Bad')
  qualityCheck <- qualityOptions[
    1:menu(title='What quality buoys should we calibrate?',
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

      # If value is not NA we have already calibrated, so dont re-cal
      if(!recalibrate && !is.na(thisBuoyData$info$CalibrationType)) {
        next
      }

      if(is.na(thisBuoyData$info$BuoyQuality)) {
        skipCount <- skipCount + 1
        next
      }
      if(!(thisBuoyData$info$BuoyQuality %in% qualityCheck)) {
        next
      }
      # Calculate all here so we can graph and use later
      methodValues <- with(thisBuoyData$calibration,
                           c('Mean'=mean(offset) %% 360,
                             'Median'=median(offset) %% 360,
                             'Magnetic'=median(magnetic.variation) %% 360))
      methodValues <- ifelse(methodValues > 180, methodValues - 360, methodValues)
      if(!useSame) {
        # Lines for draw on graph
        myLines <- data.frame(Type=factor(paste0(names(methodValues), ': ', round(methodValues, 1)),
                                          levels=paste0(names(methodValues), ': ', round(methodValues, 1))),
                              vLine = methodValues,
                              lineColor = c('purple', 'green', 'red'),
                              lineSize = c(2,2,2))
        g <- calibrationplot(thisBuoyData$calibration, myLines,
                             title=paste('Station: ', gsub('\\..*$', '', attr(stationList[[s]], 'station')),
                                         ', Buoy #:', names(stationList[[s]]$buoys[b])))
        print(g)
        methodPick <- menu(title='What calibration method should be used?',
                           choices=c('Mean', 'Median', 'Magnetic', 'End Calibration'))
        if(methodPick==4) {
          if(skipCount > 0) {
            cat('Skipped', skipCount, 'buoys with calibration data that had not been checked. \n')
          }
          cat('Applied calibration to', calibrateCount, 'buoys total.')
          return(stationList)
        } else {
          method <- calibrationMethods[methodPick]
        }
      }
      # Returning these as functions so that we have more complicated options later. So could have one
      # that takes in the bearing of the data or whatever as an argument
      calibrationFunction = switch(
        method,
        mean = function(data) methodValues[1],
        median = function(data) methodValues[2],
        magnetic = function(data) methodValues[3]
      )

      stationList[[s]]$buoys[[thisBuoy]]$info$CalibrationType <- method

      stationList[[s]]$detections[whichThisBuoy,] <- stationList[[s]]$detections[whichThisBuoy,] %>%
        mutate(CalibrationValue = calibrationFunction(.),
               CalibratedBearing = (DIFARBearing + CalibrationValue) %% 360)

      stationList[[s]]$buoys[[thisBuoy]]$calibration <- stationList[[s]]$buoys[[thisBuoy]]$calibration %>%
        mutate(CalibrationValue = calibrationFunction(.),
               CalibratedBearing = (DIFARBearing + CalibrationValue) %% 360)
      calibrateCount <- calibrateCount + 1
    }
  }
  if(skipCount > 0) {
    cat('Skipped', skipCount, 'buoys with calibration data that had not been checked. \n')
  }
  cat('Applied calibration to', calibrateCount, 'buoys total.')
  stationList
}
