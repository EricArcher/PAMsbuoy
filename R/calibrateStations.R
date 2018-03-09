#' @name calibrateStations
#' @title Calibrate Sonobuoy Stations
#' @description Perform calibration for sonobuoy stations. This can take either a list
#'   of stations or a single station as input, and will modify the object directly in
#'   the parent environment so no assignment is necessary. The user can select from three
#'   calibration options: Check calibrations, apply calibrations, or calibrate drift (these
#'   must be done in this order).
#'
#' @param stationList Either a list of stations or a single station to calibrate
#' @param type The type of calibration to perform, either \code{'check'}, \code{'apply'},
#'   or \code{'drift'}.
#' @param recalibrate Should we re-do stations we have already calibrated?
#' @param \dots Other parameters to be passed to different calibration methods
#'
#' @return Returns the stationList object with calibration information added. Modifies
#'   the object in the parent environment so no assignment is needed.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
calibrateStations <- function(stationList, type='', recalibrate=FALSE, ...) {
  varName <- deparse(substitute(stationList))
  frame <- parent.frame()
  parseName <- parseStationName(stationList=stationList, varName = varName, frame=frame)
  isSurvey <- parseName$isSurvey
  stationList <- parseName$stationList
  varName <- parseName$varName
  myStations <- parseName$myStations
  # if(is.null(attr(stationList, 'survey'))) {
  #   # If not a whole survey, should be a station
  #   # Station could either be Survey$Station, or just Station
  #   # SHIT could also be Survey[1:10]
  #   splitName <- strsplit(varName, '\\$')[[1]]
  #   if(length(splitName)==1 &&
  #      !is.na(attr(stationList, 'station'))) {
  #     isSurvey <- FALSE
  #     # This looks weird, but makes it fit with rest of cases in loop below
  #     # the isSurvey flag is so we know to undo this list
  #     stationList <- list(stationList)
  #     myStations <- 1
  #   } else if(length(splitName)==2) {
  #     isSurvey <- TRUE
  #     varName <- splitName[1]
  #     # If it was Survey$Station, we load the entire Station in from
  #     # the parent frame so that we can modify it, then rewrite it
  #     stationList <- get(varName, frame)
  #     myStations <- which(gsub('`', '', splitName[2]) == names(stationList))
  #   }
  # } else {
  #   isSurvey <- TRUE
  #   myStations <- seq_along(stationList)
  # }
  calTypes <- c('check', 'apply', 'drift')
  if(!(type %in% calTypes)) {
    type <- calTypes[menu(title='What kind of calibration would you like to do?',
                 choices=c('Check calibration quality', 'Apply calibration', 'Calibrate drift'))]
  }

  switch (type,
    check = stationList <- checkCalibrations(stationList, myStations, recalibrate, ...),
    apply = stationList <- applyCalibration(stationList, myStations, recalibrate, ...),
    drift = stationList <- driftCalibration(stationList, myStations, recalibrate, ...)
  )

  cat('\nDone with calibration.')
  # Write back out
  # Check that new object isnt a lot smaller? Maybe? What if there are plots in the old one tho
  if(isSurvey) {
    assign(varName, stationList, frame)
    return(invisible(stationList))
  } else {
    assign(varName, stationList[[1]], frame)
    return(invisible(stationList[[1]]))
  }
}

#### Plotting
calibrationplot <- function(calibrationData, lineData, title) {
  lineData[['vLine']] <- sapply(lineData[['vLine']] %% 360, function(x) {
    if(x > 180) {
      x-360
    } else x
  })
  calibrationData[['offset']] <- sapply(calibrationData[['offset']] %% 360, function(x) {
    if(x > 180) {
      x - 360
    } else x
  })
  lineData$legend <- 0
  colMap <- as.character(distinct(lineData, Type, lineColor)$lineColor)
  names(colMap) <- distinct(lineData, Type, lineColor)$Type
  ggplot(calibrationData, aes_string(x='offset')) +
    geom_histogram(binwidth=2) +
    geom_vline(data=lineData, aes(xintercept=vLine, color=Type, size=lineSize), alpha=.5,show.legend = FALSE) +
    geom_hline(data=lineData, aes(yintercept=legend, color=Type), linetype=0) +
    scale_color_manual(NULL, values=colMap, guide=guide_legend(override.aes=list(size=2, linetype=1))) +
    scale_size_identity() +
    scale_x_continuous(limits=c(-180, 180), breaks=c(-180, -90, 0, 90, 180)) +
    scale_y_continuous(limits=c(0, NA), expand=c(0,0)) +
    labs(x='Calibration Angle Error', y='Count', title=title) +
    theme(plot.title = element_text(hjust=.5))
}

# parse station name for modifying without assignment
parseStationName <- function(stationList, varName, frame) {
  # If a survey then all normal
  if(!is.null(attr(stationList, 'survey'))) {
    isSurvey <- TRUE
    myStations <- seq_along(stationList)

    # If not a survey then we need to figure out what form
    # 3 options: Station, Survey$Station, Survey[numbers]
  } else {
    # Split by $ or [
    splitName <- unlist(strsplit(varName, '[\\$\\[]'))
    # If this is length 1, should be just Station
    if(length(splitName)==1 &&
       !is.null(attr(stationList, 'station'))) {
      isSurvey <- FALSE
      # Looks weird, but this format works with rest of functions
      stationList <- list(stationList)
      myStations <- 1
      # If option Survey$Station or Survey[numbers], we need to get the entire
      # stationList so that we can modify it and rewrite it.
    } else if(length(splitName)==2) {
      isSurvey <- TRUE
      stationList <- get(splitName[1], frame)
      if(is.null(attr(stationList, 'survey'))) {
        stop('Input does not appear to be a station or survey. Please check input.')
      }
      # Case Survey$Station may look like Survey$`Station`
      if(grepl('\\$', varName)) {
        varName <- splitName[1]
        myStations <- which(gsub('`', '', splitName[2])==names(stationList))
      } else if(grepl('\\[', varName)) {
        varName <- splitName[1]
        # Ditch outer bracket and turn text '1:10' or 'c(1,3)' into numeric vector with eval/parse
        myStations <- eval(parse(text = gsub(']', '', splitName[2])))
      } else {
        # If it wasnt Station, Survey$Station, or Survey[numbers], then something is wrong.
        stop('Input does not appear to be a station or a survey. Please check input.')
      }
    } else {
      # If splitting by $ or [ returned more than 2, something is wrong.
      stop('Input does not appear to be a station or a survey. Please check input.')
    }
  }
  list(isSurvey = isSurvey,
       stationList = stationList,
       myStations = myStations,
       varName = varName)
}
