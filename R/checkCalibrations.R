#' @name checkCalibrations
#' @title Check Buoy Calibrations
#' @description Loop through histograms of buoy calibration offsets for each
#'   buoy in each station and mark the quality of the buoy as Good, Bad, or
#'   Questionable.
#'
#' @param stationList a list of sonobuoy stations as created by \code{loadStations},
#'   or a single station as created by \code{formatStation}
#' @param myStations IDs of stations to check. Needed for calibrateStations to work correctly
#' @param recalibrate should buoys that have already been checked be re-examined? If
#'   \code{FALSE}, any buoys with existing buoyQuality will be skipped over.
#' @param midFun function to use to show center of the data
#' @param goodRange range around center of data to show guidelines for good calibrations
#'
#' @return a list of sonobuoy stations with an added buoyQuality field for each buoy.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import ggplot2
#'
checkCalibrations <- function(stationList, myStations, recalibrate = FALSE, midFun=median, goodRange=10) {
  checkCount <- 0
  for(s in myStations) {
    if(is.null(attr(stationList[[s]], 'station'))) {
      warning('Object ', s, ' is not a station. Please check input.')
      next
    } else if(!('buoys' %in% names(stationList[[s]]))) {
      warning('Station ', s, ' does not have any buoys.')
      next
    }
    for(b in seq_along(stationList[[s]]$buoys)) {
      thisBuoy <- stationList[[s]]$buoys[[b]]
      if(!recalibrate) {
        if(!is.na(thisBuoy$info$buoyQuality)) {
          next
        }
      }
      if(is.null(thisBuoy$calibration)) {
        next
      }
      myData <- thisBuoy$calibration
      myMid <- midFun(myData[['offset']]) %% 360
      myMid <- ifelse(myMid > 180, myMid-360, myMid)
      midName <- deparse(substitute(midFun))
      midName <- paste0(toupper(substring(midName, 1, 1)), substring(midName, 2))
      # Lines to draw on plot, colors names sizes
      myLines <- data.frame(Type = c(paste0(midName,': ', round(myMid, 1)),
                                     paste0('\u00B1', goodRange), paste0('\u00B1', goodRange)),
                            vLine = c(myMid, myMid + goodRange, myMid - goodRange),
                            lineSize = as.integer(c(1, 2, 2)),
                            lineColor = c('red', 'green', 'green'))
      # Title for plot
      myTitle <- paste('Station: ', gsub('\\..*$', '', attr(stationList[[s]], 'station')),
                       ', Buoy #:', names(stationList[[s]]$buoys[b]))
      g <- calibrationplot(thisBuoy$calibration, myLines, title=myTitle)
      print(g)
      choices <- c('Good', 'Bad', 'Questionable', 'End Calibration Check')
      response <- menu(choices, title='Calibration quality?')
      if(response == 4) {
        checkSummary <- summary(factor(unlist(lapply(stationList, function(s) {
          lapply(s$buoys, function(b) {
            if(is.na(b$info$buoyQuality)) {
              'Not Checked'
            } else b$info$buoyQuality
          })
        })), levels=c('Good', 'Bad', 'Questionable', 'Not Checked')))
        cat('Checked ', checkCount, ' buoys total. \n', 'Summary of buoy quality check: \n',
            paste(paste0(names(checkSummary), ': ', checkSummary), collapse=', '), '\n', sep='')
        return(stationList)
      } else {
        stationList[[s]]$buoys[[b]]$info$buoyQuality <- choices[response]
        checkCount <- checkCount + 1
      }
    }
  }
  checkSummary <- summary(factor(unlist(lapply(stationList, function(s) {
    lapply(s$buoys, function(b) {
      if(is.na(b$info$buoyQuality)) {
        'Not Checked'
      } else b$info$buoyQuality
    })
  })), levels=c('Good', 'Bad', 'Questionable', 'Not Checked')))
  cat('Checked ', checkCount, ' buoys total. \n', 'Summary of buoy quality check: \n',
      paste(paste0(names(checkSummary), ': ', checkSummary), collapse=', '), '\n', sep='')
  stationList
}
