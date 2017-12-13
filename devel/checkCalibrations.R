# Check station calibrations
checkCalibrations <- function(stationList, recheck = FALSE) {
  for(s in seq_along(stationList)) {
    for(b in seq_along(stationList[[s]]$buoys)) {
      data <- stationList[[s]]$buoys[[b]]
      if(!recheck) {
        if('BuoyQuality' %in% names(data)) {
          next
        }
      }
      if(is.null(data$calibration)) {
        next
      }
      print(qplot(x=data$calibration['offset']) +
              labs(x='Calibration Offset', y='Count',
                   title=paste('Station: ',
                               gsub('\\..*$', '', attr(stationList[[s]], 'station')),
                               ', Buoy #:', names(stationList[[s]]$buoys[b]))))
      choices <- c('Good', 'Bad', 'Questionable', 'End Calibration Check')
      response <- menu(choices, title='Calibration quality?')
      if(response == 4) {
        return(stationList)
      } else {
      stationList[[s]]$buoys[[b]]$BuoyQuality <- choices[response]
      }
    }
  }
  stationList
}