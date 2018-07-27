#' @name driftCalibration
#' @title Estimate Sonobuoy Drift Rate and Direction
#' @description Return estimated drift rate and direction of a sonobuoy
#'
#' @param stationList a list of sonobuoy stations as created by \code{loadStations},
#'   or a single station as created by \code{formatStation}
#' @param myStations IDs of stations to check. Needed for calibrateStations to work correctly
#' @param recalibrate should buoys that have already been checked be re-examined? If
#'   \code{FALSE}, any buoys with existing buoyQuality will be skipped over.
#' @param map should we draw a map with ship position and estimated drift direction?
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom stats optim
#' @importFrom geosphere bearing destPoint distGeo
#' @importFrom viridisLite viridis
#'
driftCalibration <- function(stationList, myStations, recalibrate = FALSE, map = FALSE) {
  if(missing(myStations)) {
    myStations <- seq_along(stationList)
  }
  skipCount <- 0
  checkCount <- 0
  qualityOptions <- c('Good', 'Questionable', 'Bad')
  qualityCheck <- qualityOptions[
    1:menu(title='What quality buoys should we calibrate?',
           choices = c('Good only', 'Good and Questionable', 'Good, Questionable, and Bad'))]

  # Getting summary of buoy qualities to make a progress bar
  checkSummary <- summary(factor(unlist(lapply(stationList[myStations], function(s) {
    lapply(s$buoys, function(b) {
      if(is.na(b$info$buoyQuality) ||
         is.na(b$info$calibrationType) ||
        (!recalibrate && !is.na(b$info$drift$quality))) {
        'Not Checked'
      } else b$info$buoyQuality
    })
  })), levels=c('Good', 'Bad', 'Questionable', 'Not Checked')))
  totalToCheck <- sum(checkSummary[qualityCheck])
  if(totalToCheck == 0) {
    cat('Calculated drift for', checkCount, 'buoys total.\n')
    return(stationList)
  }
  cat('Calculating drift calibration... \n')
  pb <- txtProgressBar(min=0, max=totalToCheck, style=3)

  for(s in myStations) {
    if(is.null(attr(stationList[[s]], 'station'))) {
      warning('Object ', s, ' is not a station. Please check input.')
      next
    }
    for(b in seq_along(stationList[[s]]$buoys)) {
      thisBuoyData <- stationList[[s]]$buoys[[b]]
      # If this NA havent applied yet
      if(is.na(thisBuoyData$info$calibrationType)) {
        skipCount <- skipCount + 1
        next
      }
      if(!(thisBuoyData$info$buoyQuality %in% qualityCheck)) {
        next
      }
      if(!recalibrate && !is.na(thisBuoyData$info$drift)) {
        next
      }

      buoyStart <- thisBuoyData$position[1,]
      calibrationData <- thisBuoyData$calibration

      driftLike <- likeDf(start = buoyStart, boat = calibrationData)
      # Need to start optim at a reasonable value
      initialParam <- c(driftLike$rate[1], driftLike$angle[1])
      driftEst <- optim(par=initialParam, driftLogl, boat=calibrationData, start=buoyStart,
                        control=list('fnscale'=-1, maxit=10000, parscale=c(30,1)),
                        hessian=TRUE, method='L-BFGS-B', lower=c(0,0), upper=c(3,360))
      drift <- suppressWarnings(list(rate=driftEst$par[1], bearing = driftEst$par[2],
                    stdErr = sqrt(diag(solve(-driftEst$hessian)))))
      drift$errorPlot <- driftErrorPlot(driftLike, start=buoyStart, boat=calibrationData) +
        labs(title = paste0('Estimated Std. Error: ', round(drift$stdErr[1], 3)))
      drift$likePlot <- ggplot(data=driftLike, aes(x=angle, rate, fill=value)) + geom_tile() +
        scale_fill_gradientn(colors=viridis(256))
      if(map) {
        drift$mapPlot <- driftMapPlot(drift, start=buoyStart, boat=calibrationData)
      }
      # Need to add question then show plot. Need to have run through all no check mode?
      drift$quality <- 'NEEDTOASK'
      stationList[[s]]$buoys[[b]]$info$drift <- drift
      checkCount <- checkCount + 1
      setTxtProgressBar(pb, checkCount)
    }
  }
  cat('\nCalculated drift for', checkCount, 'buoys total.\n')
  stationList
}

expectedBearing <- function(boat, start, drift) {
  driftDistance <- drift[1]*(as.numeric(boat$UTC)-as.numeric(start$UTC))/3600
  buoyLoc <- geosphere::destPoint(c(start$Longitude, start$Latitude), b=drift[2], d=driftDistance*1000)
  geosphere::bearing(buoyLoc, cbind(boat$BoatLongitude, boat$BoatLatitude))
}

likeDf <- function(nAngles=30, nRates=30, FUN=driftLogl, boat, start, sd=10) {
  angles <- seq(0,360, length.out=nAngles)
  rates <- seq(0, 3, length.out=nRates)
  drift <- matrix(c(rep(rates, each=nAngles), rep(angles, nRates)), ncol=2)
  value <- FUN(boat=boat, start=start, drift=drift, sd=sd)
  # Doing this to normalize the loglike. Other wise exp(value) -> 0
  value <- value - max(value)
  df <- data.frame(cbind(drift, value))
  colnames(df) <- c('rate', 'angle', 'value')
  df %>% arrange(desc(value)) %>% mutate(expValue = exp(value))
}

driftLogl <- function(boat, start, drift, sd=10, bearing='calibratedBearing') {
  expected <- apply(matrix(drift, ncol=2), 1, expectedBearing, boat=boat, start=start)
  apply(expected, 2, function(x) {
    error <- (boat[[bearing]] - x) %% 360
    error <- ifelse(error < abs(error-360),
      error,
      error - 360)
    -1*(nrow(boat)/2)*log(2*pi*(sd^2)) - (1/2/(sd^2))*sum((error)^2)
  })
}

driftErrorPlot <- function(driftLike, boat, start, breakLevel=0.1) {
  endPoints <- geosphere::destPoint(c(start$Longitude, start$Latitude), b=driftLike$angle,
                                     d=driftLike$rate*1000)
  distances <- geosphere::distGeo(endPoints[1,], endPoints)/1000
  driftLike <- driftLike %>%
    mutate(Latitude=endPoints[,2], Longitude=endPoints[,1], errorRatio=distances/.$rate[1],
           errorBreaks = cut(errorRatio, seq(0, 2, breakLevel), ordered_result = TRUE, include.lowest = TRUE),
           expValue=expValue/sum(driftLike$expValue))
  distSummary <- group_by(driftLike, errorBreaks) %>% summarise(like = sum(expValue)) %>%
    mutate(like = like/sum(.$like), cumLike = cumsum(like), errorRatio=as.numeric(errorBreaks)*breakLevel,
           CI=cut(cumLike, c(0,.8, .9,.95,.99,1), include.lowest=TRUE))
  maxCI <- distSummary %>% group_by(CI) %>% summarise(max=max(errorRatio))

  # Creating colors and labels. This way will keep colors consistent if levels are missing.
  ciLabels <- c('[0,0.8]','(0.8,0.9]', '(0.9,0.95]', '(0.95,0.99]', '(0.99,1]')
  haveLevels <- sort(as.numeric(unique(maxCI$CI)))
  haveLabels <- ciLabels[haveLevels]
  ciColors <- c('red', 'orangered1', 'orange', 'darkgreen', 'green')
  haveColors <- ciColors[haveLevels]

  ggplot(distSummary, aes(x=errorRatio, y=like, fill=CI)) +
    geom_col(alpha=.5, position=position_nudge(x=-breakLevel/2)) +
    geom_vline(data=maxCI[-nrow(maxCI),], aes(xintercept=max, color=CI), size=2, show.legend=FALSE) +
    scale_color_manual(labels=haveLabels, values=haveColors) +
    scale_fill_manual(labels=haveLabels, values=haveColors) +
    coord_cartesian(xlim=c(0, 2), expand=FALSE) +
    scale_x_continuous(breaks=c(0, maxCI$max[1:3]))
}

driftMapPlot <- function(drift, boat, start, map=TRUE) {
  endPoints <- swfscMisc::destination(start$Latitude, start$Longitude, drift$bearing, units='km',
                                      drift$rate*difftime(boat$UTC[nrow(boat)], start$UTC, units='secs')/3600)
  start$endLat <- endPoints[1]; start$endLong <- endPoints[2]
  if(map) {
    boatMap <- getMap(data.frame(Longitude = c(boat$BoatLongitude, start$endLong),
                                 Latitude = c(boat$BoatLatitude, start$endLat)), zoom=16, quiet=TRUE)
  } else {
    boatMap <- ggplot()
  }
  boatPlot <- boatMap +
    geom_point(data=boat, aes(x=BoatLongitude, y=BoatLatitude, color='Boat Path')) +
    geom_point(data=start, aes(x=Longitude, y=Latitude, color='Buoy Start')) +
    geom_segment(data=start, aes(x=Longitude, xend=endLong, y=Latitude, yend=endLat, color='Drift Estimate'),
                 size=6, alpha=.4) +
    labs(title = paste0('Estimated Drift: ', round(drift$rate, 2), ' km/h, ', round(drift$bearing, 0), '\U00B0'),
         x = 'Longitude', y = 'Latitude')
}
