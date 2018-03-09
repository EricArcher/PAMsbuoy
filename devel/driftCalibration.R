#' @name driftCalibration
#' @title Estimate Sonobuoy Drift Rate and Direction
#' @description Return estimated drift rate and direction of a sonobuoy
#'
#' @param buoy.data list of data.frames containing buoy calibration and
#'   position data
#' @param graph flag of whether or not to create likelihood graph for diagnostics
#' @param initial vector of initial values for optim algorithm in km/h and true bearing
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
driftCalibration <- function(buoy.data, graph=FALSE, initial=c(1, 0), sd=10, ...) {
  # Check if it is just one buoy of a station, instead of the list of all buoys
  if('position' %in% names(buoy.data)) {
    buoy.data <- list(buoy.data)
    names(buoy.data) <- buoy.data[[1]]$position$Buoy[1]
  }
  lapply(buoy.data, function(buoy) {
    start <- buoy$position[1,]
    boat <- buoy$calibration
    if(graph) {
      driftDf <- likeDf(nAngles=30,nRates=30, boat=boat, start=start, sd=sd) %>%
        arrange(desc(Value))
      graph <- ggplot(driftDf, aes(x=Angle, y=Rate, fill=log(-Value))) + geom_tile() +
        scale_fill_gradientn(colors=viridis(256, direction=-1, option='magma'))
      drift <- list(rate=driftDf$Rate[1], bearing=driftDf$Angle[1], graph=graph)
    } else {
      initDrift <- likeDf(nAngles=30, nRates=30, boat=boat, start=start, sd=sd) %>%
        arrange(desc(Value)) %>% mutate(ExpValue = exp(Value))
      initial=c(initDrift$Rate[1], initDrift$Angle[1])
      drift <- optim(par=initial, driftLogl, boat=boat, start=start, sd=sd,
                     control=list('fnscale'=-1, maxit=10000, parscale=c(30,1)),
                     hessian=TRUE, method='L-BFGS-B', lower=c(0, 0), upper=c(3, 360))
      drift <- list(rate=drift$par[1], bearing=drift$par[2], err=sqrt(diag(solve(-drift$hessian))))
      print(driftErrorPlot(initDrift, boat=boat, start=start) +
              labs(title=paste0('Est. Error: ', round(drift$err[1], 3))))
    }
    print(driftMapPlot(drift, boat=boat, start=start))
    drift
  })
}

expectedBearing <- function(boat, start, drift) {
  drift.distance <- drift[1]*(as.numeric(boat$UTC)-as.numeric(start$UTC))/3600
  buoyLoc <- geosphere::destPoint(c(start$Longitude, start$Latitude), b=drift[2], d=drift.distance*1000)
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
  colnames(df) <- c('Rate', 'Angle', 'Value')
  df
}

driftLogl <- function(boat, start, drift, sd=4) {
  expected <- apply(matrix(drift, ncol=2), 1, expectedBearing, boat=boat, start=start)
  apply(expected, 2, function(x) {
    error <- (boat$DIFARBearing - x) %% 360
    error <- ifelse(error < abs(error-360),
      error,
      error - 360)
    -1*(nrow(boat)/2)*log(2*pi*(sd^2)) - (1/2/(sd^2))*sum((error)^2)
  })
}

# likeDf <- function(nAngles=60, nRates=30, FUN=driftLogl, boat, start, sd=10) {
#   angles <- seq(0,360, length.out=nAngles)
#   rates <- seq(0, 3, length.out=nRates)
#   do.call(rbind, lapply(rates, function(r) {
#     value <- sapply(angles, function(a) {
#       driftLogl(boat, start, c(r,a), sd)
#     })
#     data.frame(Rate=r, Angle=angles, Value=value)
#   }))
# }
#
# driftLogl <- function(boat, start, drift, sd=4) {
#   expected <- expectedBearing(boat, start, drift)
#   error <- sapply((boat$DIFARBearing - expected) %% 360, function(x) {
#     if(x < abs(x-360)) {x}
#     else {x-360}
#   }
#   )
#   -1*(nrow(boat)/2)*log(2*pi*(sd^2)) - (1/2/(sd^2))*sum((error)^2)
# }

driftErrorPlot <- function(driftLike, boat, start, breakLevel=0.1) {
  endPoints <- mapply(swfscMisc::destination, start$Latitude, start$Longitude, driftLike$Angle,
                      distance = driftLike$Rate, units='km')
  distances <- mapply(distance, endPoints[1,1], endPoints[2,1], endPoints[1,], endPoints[2,], units='km')
  driftLike <- driftLike %>%
    mutate(Latitude=endPoints[1,], Longitude=endPoints[2,], ErrorRatio=distances/.$Rate[1],
           ErrorBreaks = cut(ErrorRatio, seq(0, 2, breakLevel), ordered_result = TRUE, include.lowest = TRUE),
           ExpValue=ExpValue/sum(driftLike$ExpValue))
  distSummary <- group_by(driftLike, ErrorBreaks) %>% summarise(Like = sum(ExpValue)) %>%
    mutate(Like = Like/sum(.$Like), CumLike = cumsum(Like), ErrorRatio=as.numeric(ErrorBreaks)*breakLevel,
           CI=cut(CumLike, c(0,.9,.95,.99,1), include.lowest=TRUE))
  maxCI <- distSummary %>% group_by(CI) %>% summarise(Max=max(ErrorRatio))
  drift$CI90 <- maxCI$Max[1]; drift$CI95 <- maxCI$Max[2]; drift$CI99 <- maxCI$Max[3]
  ggplot(distSummary, aes(x=ErrorRatio, y=Like, fill=CI)) +
    geom_col(alpha=.5, position=position_nudge(x=-breakLevel/2)) +
    geom_vline(data=maxCI[-4,], aes(xintercept=Max, color=CI), size=2, show.legend=FALSE) +
    scale_color_manual(values=c('darkgreen', 'orange', 'red', 'red')) +
    scale_fill_manual(values=c('darkgreen', 'orange', 'red', 'red')) +
    coord_cartesian(xlim=c(0, 2), expand=FALSE) +
    scale_x_continuous(breaks=c(0, maxCI$Max[1:3]))
}

driftMapPlot <- function(drift, boat, start, map=TRUE) {
  endPoints <- swfscMisc::destination(start$Latitude, start$Longitude, drift$bearing,
                                      drift$rate*difftime(boat$UTC[nrow(boat)], start$UTC, units='secs')/3600,
                                      units='km')
  start$endLat <- endPoints[1]; start$endLong <- endPoints[2]
  if(map) {
    boatMap <- getMap(data.frame(Longitude = c(boat$BoatLongitude, start$endLong),
                                 Latitude = c(boat$BoatLatitude, start$endLat)), zoom=16, quiet=TRUE)
  } else boatMap <- ggplot()
  boatPlot <- boatMap +
    geom_point(data=boat, aes(x=BoatLongitude, y=BoatLatitude, color='Boat Path')) +
    geom_point(data=start, aes(x=Longitude, y=Latitude, color='Buoy Start')) +
    geom_segment(data=start, aes(x=Longitude, xend=endLong, y=Latitude, yend=endLat, color='Drift Estimate'),
                 size=6, alpha=.4) +
    labs(title = paste0('Estimated Drift: ', round(drift$rate, 2), ' km/h, ', round(drift$bearing, 0), '\U00B0'),
         x = 'Longitude', y = 'Latitude')
}
