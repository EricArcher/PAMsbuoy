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
#' @return a list of sonobuoy stations with an added BuoyQuality field for each buoy.
#'   Will also directly modify the calStations object even if the result is not
#'   assigned to a variable.
#'
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom ggplot2 qplot
#' @export
#'
checkCalibrations <- function(stationList, recheck = FALSE, goodRange = 10, midFun=median) {
  # Doing some janky stuff to be able to modify stationList in the
  # parent environment so that assignment is not needed when calling.
  # This is to make changing a single station within a list easier /
  # harder to mess up.

  # Gets the character string name of stationList input and frame where we called
  varName <- deparse(substitute(stationList))
  frame <- parent.frame()
  if(is.null(attr(stationList, 'survey'))) {
    # If not a whole survey, should be a station
    # Station could either be Survey$Station, or just Station
    # Depending on how it was loaded (maybe just called formatStation
    # on a single sqlite)
    splitName <- strsplit(varName, '\\$')[[1]]
    if(length(splitName)==1) {
      isSurvey <- FALSE
      # This looks weird, but makes it fit with rest of cases in loop below
      # the isSurvey flag is so we know to undo this list
      stationList <- list(stationList)
      myStations <- 1
    } else if(length(splitName)==2) {
      isSurvey <- TRUE
      varName <- splitName[1]
      # If it was Survey$Station, we load the entire Station in from
      # the parent frame so that we can modify it, then rewrite it
      stationList <- get(varName, frame)
      myStations <- which(gsub('`', '', splitName[2]) == names(stationList))
    }
  } else {
    isSurvey <- TRUE
    myStations <- seq_along(stationList)
  }

  # Loop through every station we are interested in and look at calibrations
  for(s in myStations) {
    if(is.null(attr(stationList[[s]], 'station'))) {
      warning(varName, ' is not a list of stations. Please check input.')
      next
    } else if(!('buoys' %in% names(stationList[[s]]))) {
      warning('Station ', varName, '[[', s, ']]', ' does not have any buoys.')
      next
    }
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

      myData <- thisBuoy$calibration
      myMid <- midFun(myData[['offset']])
      midName <- deparse(substitute(midFun))
      midName <- paste0(toupper(substring(midName, 1, 1)), substring(midName, 2))
      myLines <- data.frame(Type = c('Center', 'Bound', 'Bound'),
                            vLine = c(myMid, myMid + goodRange, myMid - goodRange),
                            hLine = 0)
      myLines[['vLine']] <- sapply(myLines[['vLine']] %% 360, function(x) {
        if(x > 180) {
          x-360
        } else x
      })
      g <- ggplot(myData, aes_string(x='offset')) +
        geom_histogram(binwidth=2) +
        geom_vline(data=myLines, aes(xintercept=vLine, color=Type, size=Type), alpha=.5,show.legend = FALSE) +
        geom_hline(data=myLines, aes(yintercept=hLine, color=Type), linetype=0) +
        scale_color_manual('Range', values=c('green', 'red'),
                           labels=c(paste0('\u00B1', goodRange),
                                    paste0(midName,': ', round(myMid, 1))),
                           guide=guide_legend(override.aes=list(size=2, linetype=1))) +
        scale_size_manual(values=c(2,1), guide='none') +
        scale_x_continuous(limits=c(-180, 180), breaks=c(-180, -90, 0, 90, 180)) +
        labs(x='Calibration Angle Error', y='Count',
             title=paste('Station: ', gsub('\\..*$', '', attr(stationList[[s]], 'station')),
                         ', Buoy #:', names(stationList[[s]]$buoys[b]))) +
        theme(plot.title = element_text(hjust=.5))
      print(g)
      choices <- c('Good', 'Bad', 'Questionable', 'End Calibration Check')
      response <- menu(choices, title='Calibration quality?')
      if(response == 4) {
        if(isSurvey) {
          assign(varName, stationList, frame)
          return(invisible(stationList))
        } else {
          assign(varName, stationList[[1]], frame)
          return(invisible(stationList[[1]]))
        }
      } else {
      stationList[[s]]$buoys[[b]]$BuoyQuality <- choices[response]
      }
    }
  }
  if(isSurvey) {
    assign(varName, stationList, frame)
    return(invisible(stationList))
  } else {
    assign(varName, stationList[[1]], frame)
    return(invisible(stationList[[1]]))
  }
}