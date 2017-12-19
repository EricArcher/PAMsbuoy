#' @title Check Buoy Calibrations
#' @description Loop through histograms of buoy calibration offsets for each
#'   buoy in each station and mark the quality of the buoy as Good, Bad, or
#'   Questionable.
#'
#' @param stationList a list of sonobuoy stations as created by \code{loadStations},
#'   or a single station as created by \code{formatStation}
#' @param recheck should buoys that have already been checked be re-examined? If
#'   \code{FALSE}, any buoys with existing BuoyQuality will be skipped over.
#'
#' @return a list of sonobuoy stations with an added BuoyQuality field for each buoy
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom ggplot2 qplot
#' @export
#'
checkCalibrations <- function(stationList, recheck = FALSE) {
  # Checking if this is an entire survey or just one station
  if(is.null(attr(stationList, 'survey'))) {
    isSurvey <- FALSE
    # If just a station, do this so seq_along works in the first for loop
    stationList <- list(stationList)
  } else {
    isSurvey <- TRUE
  }

  for(s in seq_along(stationList)) {
    for(b in seq_along(stationList[[s]]$buoys)) {
      thisBuoy <- stationList[[s]]$buoys[[b]]
      if(!recheck) {
        if('BuoyQuality' %in% names(thisBuoy)) {
          next
        }
      }
      if(is.null(thisBuoy$calibration)) {
        next
      }
      print(qplot(x=thisBuoy$calibration['offset']) +
              labs(x='Calibration Offset', y='Count',
                   title=paste('Station: ',
                               gsub('\\..*$', '', attr(stationList[[s]], 'station')),
                               ', Buoy #:', names(stationList[[s]]$buoys[b]))))
      choices <- c('Good', 'Bad', 'Questionable', 'End Calibration Check')
      response <- menu(choices, title='Calibration quality?')
      if(response == 4) {
        if(isSurvey) {
          return(stationList)
        } else {
          return(stationList[[1]])
        }
      } else {
      stationList[[s]]$buoys[[b]]$BuoyQuality <- choices[response]
      }
    }
  }
  if(isSurvey) {
    return(stationList)
  } else {
    return(stationList[[1]])
  }
}