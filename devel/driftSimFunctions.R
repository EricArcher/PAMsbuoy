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
  dist1 <- seq(distances[1]/nPoints, distances[1], length.out=nPoints)
  points1 <- do.call(rbind, lapply(dist1, function(d) {
    swfscMisc::destination(start$Latitude, start$Longitude, angle, d, units='km')
  }))
  points1 <- data.frame(BoatLatitude=points1[,1], BoatLongitude=points1[,2],
                        UTC=dist1/boatKnots/1.85*3600+start$UTC)
  dist2 <- seq(distances[2]/nPoints, distances[2], length.out=nPoints)
  points2 <- do.call(rbind, lapply(dist2, function(d) {
    swfscMisc::destination(points1$BoatLatitude[nPoints], points1$BoatLongitude[nPoints],
                           angle+turn, d, units='km')
  }))
  points2 <- data.frame(BoatLatitude=points2[,1], BoatLongitude=points2[,2],
                        UTC=points1$UTC[nPoints] + dist2/boatKnots/1.85*3600)
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
  rateHist <- ggplot(driftData, aes(x=Rate)) + geom_histogram(binwidth=.1) +
    geom_vline(xintercept=rate, size=2, color='green') + xlim(0,3.2)
  bearingHist <- ggplot(driftData, aes(x=Bearing)) + geom_histogram(binwidth=2) +
    geom_vline(xintercept=bearing, size=2, color='green') + xlim(0,360)
  endings <- mapply(destination, start$Latitude, start$Longitude, driftData$Bearing,
                    driftData$Rate*time/3600, units='km')
  realEnd <- destination(start$Latitude, start$Longitude, bearing, rate*time/3600, units='km')
  distError <- mapply(distance, realEnd[1], realEnd[2], endings[1,], endings[2,], units='km')
  distHist <- ggplot(data.frame(Distance=distError), aes(x=Distance)) + geom_histogram(binwidth=.02) +
    xlim(0, max(distError, rate*time/3600)) + geom_vline(xintercept=rate*time/3600, size=2, color='green') +
    labs(title=as.character(rate*time/3600))
  endDf <- data.frame(Latitude=endings[1,], Longitude=endings[2,])
  driftPlot <- ggplot(endDf) +
    geom_segment(aes(x=start$Longitude, y=start$Latitude, xend=Longitude, yend=Latitude), color='blue', alpha=.2) +
    geom_segment(x=start$Longitude, y=start$Latitude, xend=realEnd[2], yend=realEnd[1], color='green', size=3)
  grid.arrange(rateHist, bearingHist, distHist, driftPlot, nrow=2)
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
                   numInit=5, numGrid=60, angleError=0, angleBias=0, modelSd = 10) {
  buoy <- driftBuoy(start, rate, bearing, times=boat$UTC)
  boat <- makeDifar(boat, buoy) %>% mutate(DIFARBearing = DIFARBearing + rnorm(nrow(boat), angleBias, angleError))
  initDrift <- likeDf(nAngles=numInit, nRates=numInit, boat=boat, start=start) %>%
    arrange(desc(Value))
  drift <- driftCalibration(list(position=start, calibration=boat),
                            initial=c(initDrift$Rate[1], initDrift$Angle[1]))[[1]]
  endPoints <- swfscMisc::destination(start$Latitude, start$Longitude, drift$bearing,
                                drift$rate*difftime(boat$UTC[nrow(boat)], start$UTC, units='secs')/3600,
                                units='km')
  start$endLat <- endPoints[1]; start$endLong <- endPoints[2]
  boatMap <- getMap(rename(boat, Latitude=BoatLatitude, Longitude=BoatLongitude), zoom=16)
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
      arrange(desc(Value)) %>% mutate(Value = Value/nrow(boat))
    debugPoints <- data.frame(Angle = c(initDrift$Angle[1], drift$bearing[1], driftLike$Angle[1], bearing),
                              Rate = c(initDrift$Rate[1], drift$rate[1], driftLike$Rate[1], rate),
                              Name = c('Initial', 'Drift Estimate', 'Grid Estimate', 'Actual'))
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
      geom_point(data=debugPoints, aes(x=Angle, y=Rate, color=Name), size=3)
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
      print(gridExtra::grid.arrange(boatPlot, anglePlot, likePlot, nrow=2))
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

##################### BIASED VERSION ########################
driftCalibrationBias <- function(buoy.data, graph=FALSE, initial=c(1, 0, 0), ...) {
  # Check if it is just one buoy of a station, instead of the list of all buoys
  if('position' %in% names(buoy.data)) {
    buoy.data <- list(buoy.data)
    names(buoy.data) <- buoy.data[[1]]$position$Buoy[1]
  }
  lapply(buoy.data, function(buoy) {
    start <- buoy$position[1,]
    if(graph) {
      driftDf <- likeDf(nAngles=30,nRates=30, boat=boatLines, start=start) %>%
        arrange(desc(Value))
      ggplot(driftDf, aes(x=Angle, y=Rate, color=log(-Value))) + geom_point(size=8) +
        scale_color_gradientn(colors=viridis(256, direction=-1, option='magma'))

    } else {
      drift <- optim(par=initial, driftLoglBias, boat=buoy$calibration, start=start,
                     control=list('fnscale'=-1, maxit=10000, parscale=c(30,1, 5)),
                     hessian=TRUE, method='L-BFGS-B', lower=c(0, 0, -20), upper=c(3, 360, 20))
    }
    list(rate=drift$par[1], bearing=drift$par[2], bias=drift$par[3], hessian=drift$hessian)
  })
}

driftLoglBias <- function(boat, start, drift, sd=4) {
  # Drift is rate, bearing , bias
  expected <- expectedBearing(boat, start, drift[1], drift[2])
  error <- sapply((boat$DIFARBearing - expected - drift[3]) %% 360, function(x) {
    if(x < abs(x-360)) {x}
    else {x-360}
  }
  )
  -1*(nrow(boat)/2)*log(2*pi*(sd^2)) - (1/2/(sd^2))*sum((error)^2)
}

testitbias <- function(boat, start, rate, bearing, plot=FALSE, like=FALSE, debug=FALSE,
                   numInit=5, numGrid=60, angleError=0, angleBias=0, modelSd = 10) {
  buoy <- driftBuoy(start, rate, bearing, times=boat$UTC)
  boat <- makeDifar(boat, buoy) %>% mutate(DIFARBearing = DIFARBearing + rnorm(nrow(boat), angleBias, angleError))
  # initDrift <- likeDf(nAngles=numInit, nRates=numInit, boat=boat, start=start) %>%
  #   arrange(desc(Value))
  drift <- driftCalibrationBias(list(position=start, calibration=boat), initial = c(1,100,0))[[1]]
  endPoints <- swfscMisc::destination(start$Latitude, start$Longitude, drift$bearing,
                                drift$rate*difftime(boat$UTC[nrow(boat)], start$UTC, units='secs')/3600,
                                units='km')
  boatPlot <- ggplot() + geom_point(data=boat, aes(x=BoatLongitude, y=BoatLatitude, color='Boat Path')) +
    geom_point(data=buoy, aes(x=Longitude, y=Latitude, color='Buoy Path')) +
    geom_segment(aes(x=start$Longitude, xend=endPoints[2], y=start$Latitude, yend=endPoints[1], color='Drift Estimate'),
                 size=6, alpha=.4) +
    labs(title=paste0('Angle Error: ', angleError, '. Angle Bias: ', angleBias),
         x='Longitude', y='Latitude') + theme(plot.title=element_text(hjust=.5))
  if(like) {
    # browser()
    driftLike <- likeDf(nAngles=numGrid, nRates=numGrid, boat=boat, start=start, sd=modelSd) %>%
      arrange(desc(Value)) %>% mutate(Value = Value/nrow(boat))
    debugPoints <- data.frame(Angle = c(initDrift$Angle[1], drift$bearing[1], driftLike$Angle[1], bearing),
                              Rate = c(initDrift$Rate[1], drift$rate[1], driftLike$Rate[1], rate),
                              Name = c('Initial', 'Drift Estimate', 'Grid Estimate', 'Actual'))
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
      geom_point(data=debugPoints, aes(x=Angle, y=Rate, color=Name), size=3)
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
      print(gridExtra::grid.arrange(boatPlot, anglePlot, likePlot, nrow=2))
    } else if(debug) {
      print(gridExtra::grid.arrange(boatPlot, anglePlot, miniLikePlot, nrow=2))
    } else {
      print(gridExtra::grid.arrange(boatPlot, anglePlot, nrow=1))
    }
  }
  drift$Range <- c(range(boat$DrawBearing), range(boat$DrawBearing)[2] - range(boat$DrawBearing)[1])
  drift
}


