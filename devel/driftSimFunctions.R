# Drift Simulation Functions
library(ggplot2)
library(dplyr)
library(viridisLite)
library(plotly)
library(gridExtra)
library(swfscMisc)

makeCircle <- function(start, center, distance, shape = 0, boatKnots = 10,
                       angles=seq(from=0, to=360, length.out=360)) {
  c <- distance*shape
  dist <- sapply(angles, function(theta) {
    (-distance^2 + c^2)/(-distance+c*cos((90-theta)*pi/180))
  })
  points <- do.call(rbind, lapply(seq_along(angles), function(a) {
    start <- swfscMisc::destination(center[1], center[2], 270, c, units='km')
    swfscMisc::destination(start[1], start[2], angles[a], dist[a], units='km')
  }))
  circ <- data.frame(BoatLatitude = points[,1], BoatLongitude = points[,2], Distance = dist,
                     TimeDiff = cumsum(dist*(angles[2]-angles[1])/boatKnots/1.85*3600*pi/180))
  circ$TimeDiff <- circ$TimeDiff - circ$TimeDiff[1]
  circ$TimeDiff <- circ$TimeDiff +
    swfscMisc::distance(start$Latitude, start$Longitude,
                        circ$BoatLatitude[1], circ$BoatLongitude[1], units='km')/boatKnots/1.85*3600
  circ$UTC <- start$UTC + circ$TimeDiff
  circ
}

makeLines <- function(start, distances, boatKnots, angle, turn=135, nPoints) {
  if(length(nPoints)==1) {
    nPoints <- rep(nPoints,2)
  }
  dist1 <- seq(distances[1]/nPoints[1], distances[1], length.out=nPoints[1])
  points1 <- do.call(rbind, lapply(dist1, function(d) {
    swfscMisc::destination(start$Latitude, start$Longitude, angle, d, units='km')
  }))
  points1 <- data.frame(BoatLatitude=points1[,1], BoatLongitude=points1[,2],
                        UTC=dist1/boatKnots/1.85*3600+start$UTC)
  dist2 <- seq(distances[2]/nPoints[2], distances[2], length.out=nPoints[2])
  points2 <- do.call(rbind, lapply(dist2, function(d) {
    swfscMisc::destination(points1$BoatLatitude[nPoints[1]], points1$BoatLongitude[nPoints[1]],
                           angle+turn, d, units='km')
  }))
  points2 <- data.frame(BoatLatitude=points2[,1], BoatLongitude=points2[,2],
                        UTC=points1$UTC[nPoints[1]] + dist2/boatKnots/1.85*3600)
  distinct(rbind(points1, points2))
}

driftBuoy <- function(start, rate, bearing, times) {
  distances <- difftime(times, start$UTC, units='secs')*rate/3600
  points <- do.call(rbind, lapply(distances, function(d) {
    swfscMisc::destination(start$Latitude, start$Longitude, bearing, d, units='km')
  }))
  data.frame(Latitude=points[,1], Longitude=points[,2], UTC=times)
}

makeDifar <- function(boat, buoy) {
  boat$Buoy <- 0
  boat$DIFARBearing <- mapply(swfscMisc::bearing, buoy$Latitude, buoy$Longitude,
                              boat$BoatLatitude, boat$BoatLongitude)[1,]
  boat
}

simDiagnostic <- function(start, driftData, rate, bearing, time=60*10) {
  rateHist <- ggplot(driftData, aes(x=Rate, fill=RateCI2)) + geom_histogram(binwidth=.1) +
    geom_vline(xintercept=rate, size=2, color='green', alpha=.5) + xlim(0,3.2) +
    labs(title=as.character(sum(driftData$RateCI2, na.rm=TRUE)/nrow(driftData)))

  bearingHist <- ggplot(driftData, aes(x=Bearing, fill=BearingCI2)) + geom_histogram(binwidth=2) +
    geom_vline(xintercept=bearing, size=2, color='green', alpha=.5) + xlim(0,360) +
    labs(title=as.character(sum(driftData$BearingCI2, na.rm=TRUE)/nrow(driftData)))

  rateErrHist <- ggplot(driftData, aes(x=RateErr)) + geom_histogram(binwidth=.002) + xlim(0,1)

  bearingErrHist <- ggplot(driftData, aes(x=BearingErr)) + geom_histogram(binwidth=.25) + xlim(0,90)

  endings <- mapply(destination, start$Latitude, start$Longitude, driftData$Bearing,
                    driftData$Rate*time/3600, units='km')
  realEnd <- destination(start$Latitude, start$Longitude, bearing, rate*time/3600, units='km')
  distError <- mapply(distance, realEnd[1], realEnd[2], endings[1,], endings[2,], units='km')
  distDf <- data.frame(Distance=distError) %>%
    arrange(Distance) %>% mutate(Percent=1/n(), CumSum=cumsum(Percent),
                                 CI = cut(CumSum, c(0,.9,.95,.99,1), include.lowest=TRUE))
  maxCI <- distDf %>% group_by(CI) %>% summarise(Max=max(Distance))

  distHist <- ggplot(distDf, aes(x=Distance, fill=CI)) +
    geom_histogram(binwidth=.05, position=position_nudge(x=-.05/2), alpha=.5) +
    # xlim(0, max(distError, rate*time/3600)) +
    # geom_vline(xintercept=rate*time/3600, size=2, color='green') +
    geom_vline(data=maxCI, aes(xintercept=Max, color=CI)) +
    scale_color_manual(values=c('darkgreen', 'orange', 'red', 'red')) +
    scale_fill_manual(values=c('darkgreen', 'orange', 'red', 'red')) +
    scale_x_continuous(breaks=c(0,1,2, round(maxCI$Max[1:3], 2))) +
    labs(title=as.character(rate*time/3600))

  endDf <- data.frame(Latitude=endings[1,], Longitude=endings[2,])
  driftPlot <- ggplot(endDf) +
    geom_point(aes(x=Longitude, y=Latitude), color='blue', size=2, alpha=.4) +
    geom_point(aes(x=realEnd[2], y=realEnd[1]), color='darkgreen', size=4) +
    geom_point(aes(x=start$Longitude, y=start$Latitude), color='green', size=4)
    # geom_segment(aes(x=start$Longitude, y=start$Latitude, xend=Longitude, yend=Latitude), color='blue', alpha=.2) +
    # geom_segment(x=start$Longitude, y=start$Latitude, xend=realEnd[2], yend=realEnd[1], color='green', size=3)
  grid.arrange(rateHist, bearingHist, distHist, driftPlot, rateErrHist, bearingErrHist, nrow=2)
}

likeDf <- function(nAngles=60, nRates=30, FUN=driftLogl, boat, start, sd=10) {
  angles <- seq(0,360, length.out=nAngles)
  rates <- seq(0, 3, length.out=nRates)
  do.call(rbind, lapply(rates, function(r) {
    value <- sapply(angles, function(a) {
      driftLogl(boat, start, c(r,a),sd)
    })
    data.frame(Rate=r, Angle=angles, Value=value)
  }))
}
testit <- function(boat, start, rate, bearing, plot=FALSE, like=FALSE, debug=FALSE,
                   numInit=5, numGrid=60, angleError=0, angleBias=0, modelSd = 10, map=FALSE) {
  buoy <- driftBuoy(start, rate, bearing, times=boat$UTC)
  boat <- makeDifar(boat, buoy) %>% mutate(DIFARBearing = DIFARBearing + rnorm(nrow(boat), angleBias, angleError))
  initDrift <- likeDf(nAngles=numInit, nRates=numInit, boat=boat, start=start) %>%
    arrange(desc(Value))
  drift <- driftCalibration(list(position=start, calibration=boat), sd=modelSd,
                            initial=c(initDrift$Rate[1], initDrift$Angle[1]))[[1]]
  endPoints <- swfscMisc::destination(start$Latitude, start$Longitude, drift$bearing,
                                drift$rate*difftime(boat$UTC[nrow(boat)], start$UTC, units='secs')/3600,
                                units='km')
  start$endLat <- endPoints[1]; start$endLong <- endPoints[2]
  if(map) {
    boatMap <- getMap(rename(boat, Latitude=BoatLatitude, Longitude=BoatLongitude), zoom=16)
  } else boatMap <- ggplot()
  # boatPlot <-ggplot() +
  boatPlot <- boatMap +
    geom_point(data=boat, aes(x=BoatLongitude, y=BoatLatitude, color='Boat Path')) +
    geom_point(data=buoy, aes(x=Longitude, y=Latitude, color='Buoy Path')) +
    geom_segment(data=start, aes(x=Longitude, xend=endLong, y=Latitude, yend=endLat, color='Drift Estimate'),
                 size=6, alpha=.4) +
    labs(title=paste0('Angle Error: ', angleError, '. Angle Bias: ', angleBias),
         x='Longitude', y='Latitude') + theme(plot.title=element_text(hjust=.5))

  if(like) {
    # browser()
    driftLike <- likeDf(nAngles=numGrid, nRates=numGrid, boat=boat, start=start, sd=modelSd) %>%
      arrange(desc(Value)) %>% mutate(ExpValue = exp(Value), Value = Value/nrow(boat))
    ### DIST STUFF ###
    breakLevel <- 0.05
    time <- 60*60
    endPoints <- mapply(swfscMisc::destination, start$Latitude, start$Longitude, driftLike$Angle,
                        distance = driftLike$Rate*time/3600, units='km')
    distances <- mapply(distance, endPoints[1,1], endPoints[2,1], endPoints[1,], endPoints[2,], units='km')
    driftLike <- mutate(driftLike, Latitude=endPoints[1,], Longitude=endPoints[2,], Distance=distances,
                        DistBreaks = cut(Distance, seq(0, max(Distance)+breakLevel, breakLevel), ordered_result = TRUE, include.lowest = TRUE),
                        ExpValue=ExpValue/sum(driftLike$ExpValue))
    distSummary <- group_by(driftLike, DistBreaks) %>% summarise(Like = sum(ExpValue)) %>%
      mutate(Like = Like/sum(.$Like), CumLike = cumsum(Like), Distance=as.numeric(DistBreaks)*breakLevel,
             CI=cut(CumLike, c(0,.9,.95,.99,1), include.lowest=TRUE))
    maxCI <- distSummary %>% group_by(CI) %>% summarise(Max=max(Distance))

    distancePlot <- ggplot(distSummary, aes(x=Distance, y=Like, fill=CI)) + geom_col(alpha=.5, position=position_nudge(x=-breakLevel/2)) +
      geom_vline(data=maxCI[-4,], aes(xintercept=Max, color=CI), size=2, show.legend=FALSE) +
      scale_color_manual(values=c('darkgreen', 'orange', 'red', 'red')) +
      scale_fill_manual(values=c('darkgreen', 'orange', 'red', 'red')) +
      coord_cartesian(xlim=c(0,1), expand=FALSE) +
      scale_x_continuous(breaks=c(0, maxCI$Max[1:3]))
    # endPoints <- geosphere::destPoint(c(start$Longitude, start$Latitude), driftLike$Angle, driftLike$Rate*time/3.6)
    # distances <- geosphere::distGeo(endPoints, endPoints[1,])
    # driftLike %>% mutate(Latitude=endPoints[,2], Longitude=endPoints[,1], Distance = distances)
    ########
    debugPoints <- data.frame(Angle = c(initDrift$Angle[1], drift$bearing[1], driftLike$Angle[1], bearing),
                              Rate = c(initDrift$Rate[1], drift$rate[1], driftLike$Rate[1], rate),
                              Name = c('Initial', 'Drift Estimate', 'Grid Estimate', 'Actual'))
    errPoints <- data.frame(StartRate=c(drift$rate[1]-drift$err[1], drift$rate[1]-2*drift$err[1], rep(drift$rate[1], 2)),
                            StartAngle=c(rep(drift$bearing[1], 2), drift$bearing[1]-drift$err[2], drift$bearing[1]-2*drift$err[2]),
                            EndRate=c(drift$rate[1]+drift$err[1], drift$rate[1]+2*drift$err[1], rep(drift$rate[1], 2)),
                            EndAngle=c(rep(drift$bearing[1], 2), drift$bearing[1]+drift$err[2], drift$bearing[1]+2*drift$err[2]))
    contours <- sapply(c(.5,1:3), function(x) log(dnorm(x*modelSd, 0, modelSd)))
    likePlot <- ggplot() + geom_tile(data=driftLike, aes(x=Angle, y=Rate, fill=Value)) +
      scale_fill_gradientn(colors=viridis(256)) +
      coord_cartesian(xlim=c(0,360), ylim=c(0,3), expand=FALSE) +
      geom_contour(data=driftLike, aes(x=Angle, y=Rate, z=Value),
                   breaks=contours[1], color='black', size=1) +
      geom_contour(data=driftLike, aes(x=Angle, y=Rate, z=Value),
                   breaks=contours[2], color='green', size=2) +
      geom_contour(data=driftLike, aes(x=Angle, y=Rate, z=Value),
                   breaks=contours[3], color='darkgreen', size=2) +
      geom_contour(data=driftLike, aes(x=Angle, y=Rate, z=Value),
                   breaks=contours[4], color='red', size=2) +
      # geom_contour(data=driftLike, aes(x=Angle, y=Rate, z=Distance),
      #              breaks=c(.1, .5, 1, 1.5, 2)) +
      geom_point(data=debugPoints, aes(x=Angle, y=Rate, color=Name), size=3) +
      geom_segment(data=errPoints, aes(x=StartAngle, y=StartRate, xend=EndAngle, yend=EndRate), size=rep(2:1,2), alpha=.5)
    drift$Like <- head(driftLike)
    drift$Contours <- contours
  }
  boat <- mutate(boat, DrawBearing = (DIFARBearing - median(boat$DIFARBearing)) %% 360,
                 DrawBearing = ifelse(abs(DrawBearing) > 180, DrawBearing - 360, DrawBearing))
  anglePlot <- ggplot(boat, aes(x=DIFARBearing)) + geom_histogram(binwidth=2) + xlim(0,360) +
    labs(title='Angles Used for Calibration') + theme(plot.title=element_text(hjust=.5))
  if(plot) {
    if(debug) {
      drift$Init <- head(initDrift)
      miniLikePlot <- ggplot(initDrift, aes(x=Angle, y=Rate, z=Value, fill=Value)) + geom_tile() +
        scale_fill_gradientn(colors=viridis(5)) +
        coord_cartesian(xlim=c(0,360), ylim=c(0,3), expand=FALSE)
    }
    if(like & debug) {
      print(gridExtra::grid.arrange(boatPlot, anglePlot, miniLikePlot, likePlot, nrow=2))
    } else if(like) {
      print(gridExtra::grid.arrange(boatPlot, anglePlot, likePlot, distancePlot, nrow=2))
    } else if(debug) {
      print(gridExtra::grid.arrange(boatPlot, anglePlot, miniLikePlot, nrow=2))
    } else {
      # print(gridExtra::grid.arrange(boatPlot, anglePlot, nrow=1))
      print(boatPlot)
    }
  }
  drift$Range <- c(range(boat$DrawBearing), range(boat$DrawBearing)[2] - range(boat$DrawBearing)[1])
  drift
}


# Distance dist function
distanceDistribution <- function(boat, start, rate=.7, bearing=130, angleError=5, angleBias=0, reps=20, time=60*60) {
  buoy <- driftBuoy(start, rate, bearing, times=boat$UTC)
  boat <- makeDifar(boat, buoy) %>% mutate(DIFARBearing = DIFARBearing + rnorm(nrow(boat), angleBias, angleError))
  driftLike <- likeDf(boat=boat, start=start, nRates=reps, nAngles=reps, sd=10) %>%
    arrange(desc(Value)) %>% mutate(Value = exp(Value))
  endPoints <- mapply(swfscMisc::destination, start$Latitude, start$Longitude, driftLike$Angle,
                                     distance = driftLike$Rate*time/3600, units='km')
  distances <- mapply(distance, endPoints[1,1], endPoints[2,1], endPoints[1,], endPoints[2,], units='km')
  driftLike %>% mutate(Latitude=endPoints[1,], Longitude=endPoints[2,], Distance=distances)
  # endPoints <- geosphere::destPoint(c(start$Longitude, start$Latitude), driftLike$Angle, driftLike$Rate*time/3.6)
  # distances <- geosphere::distGeo(endPoints, endPoints[1,])
  # driftLike %>% mutate(Latitude=endPoints[,2], Longitude=endPoints[,1], Distance = distances)
}

