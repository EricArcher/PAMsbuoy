# Drift Simulated Testing
library(ggplot2)
library(dplyr)
library(viridisLite)
library(plotly)
library(gridExtra)

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
  dist1 <- seq(0, distances[1], length.out=nPoints)
  points1 <- do.call(rbind, lapply(dist1, function(d) {
    swfscMisc::destination(start$Latitude, start$Longitude, angle, d, units='km')
  }))
  points1 <- data.frame(BoatLatitude=points1[,1], BoatLongitude=points1[,2],
                        UTC=dist1/boatKnots/1.85*3600+start$UTC)
  dist2 <- seq(0, distances[2], length.out=nPoints)
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

center <- c(32, -117); distance <- 1.5; shape=0; angles=seq(from=1, to=360, length.out=360); boatKnots=10

start <- data.frame(Latitude=32, Longitude=-117, UTC=as.POSIXct('2017-08-08 08:00:00'), Buoy=0)

boatCircle <- makeCircle(start=start, center=start, distance=1.5,
                         angles=seq(from=1, to=195, length.out=180), boatKnots=10) %>%
  makeDifar(buoy)

boatLines <- makeLines(start=start, distances=c(1,2), boatKnots=10, angle=0,turn=135,nPoints=20)

buoy <- driftBuoy(start, rate=2, bearing=130, times=boatCircle$UTC)

makeDifar <- function(boat, buoy) {
  boat$Buoy <- 0
  boat$DIFARBearing <- mapply(swfscMisc::bearing, buoy$Latitude, buoy$Longitude,
                              boat$BoatLatitude, boat$BoatLongitude)[1,]
  boat
}
boatLines <- makeDifar(boatLines, start)


driftDf <- likeDf(nAngles=60,nRates=60, boat=boatCircle, start=start) %>%
  arrange(desc(Value))

ggplot(driftDf, aes(x=Angle, y=Rate, z=Value, fill=Value)) + geom_tile() +
  scale_fill_gradientn(colors=viridis(256, direction = 1)) +
  coord_cartesian(xlim=c(0,360), ylim=c(0,3), expand=FALSE) +
  geom_contour(breaks=mean(driftDf$Value))

driftCalibration(list(position=start, calibration=boatLines))

testit <- function(boat, start, rate, bearing, plot=FALSE, like=FALSE, debug=FALSE,
                   numInit=5, numGrid=60, angleError=0, angleBias=0, modelSd = 10) {
  buoy <- driftBuoy(start, rate, bearing, times=boat$UTC)
  boat <- makeDifar(boat, buoy) %>% mutate(DIFARBearing = DIFARBearing + rnorm(nrow(boat), angleBias, angleError))
  initDrift <- likeDf(nAngles=numInit, nRates=numInit, boat=boat, start=start) %>%
     arrange(desc(Value))
  drift <- driftCalibration(list(position=start, calibration=boat),
                            initial=c(initDrift$Rate[1], initDrift$Angle[1]))[[1]]
  end <- swfscMisc::destination(start$Latitude, start$Longitude, drift$bearing,
                                drift$rate*difftime(boat$UTC[nrow(boat)], start$UTC, units='secs')/3600,
                                units='km')
  boatPlot <- ggplot() + geom_point(data=boat, aes(x=BoatLongitude, y=BoatLatitude, color='Boat')) +
    geom_point(data=buoy, aes(x=Longitude, y=Latitude, color='Buoy')) +
    geom_segment(aes(x=start$Longitude, xend=end[2], y=start$Latitude, yend=end[1], color='Drift'),
                 size=6, alpha=.4) +
    labs(title=paste0('Angle error: ', angleError, '. Bias: ', angleBias))
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
  anglePlot <- ggplot(boat, aes(x=DrawBearing)) + geom_histogram(binwidth=2) + xlim(-180,180)
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

testDat <- makeCircle(start=start, center=start, distance=1.5, angles=seq(from=260, to=360, length.out=20), boatKnots=10)
testit(testDat, start, 2, 130, plot=TRUE, like=FALSE, debug=FALSE, numInit = 12, numGrid=30,
       angleError=5, angleBias=0, modelSd=10)

testDat <- makeLines(start=start, distances=c(1,1.5), boatKnots=10, angle=90, turn=150, nPoints=20)
testit(testDat, start, 1.5, 130, plot=TRUE, like=TRUE, debug=F, numInit = 12, numGrid=50,
       angleError=10, angleBias=5, modelSd=10)

# ggplot() + geom_point(data=boatLines, aes(x=BoatLongitude, y=BoatLatitude, color='Boat')) +
#   geom_point(data=buoy, aes(x=Longitude, y=Latitude, color='Buoy'))
#
# boatCirc <- makeCircle(center=center, distance=distance, shape=shape, boatKnots=boatKnots, angles=angles)
# dists <- mapply(swfscMisc::distance, center[1], center[2], boatCirc$Latitude, boatCirc$Longitude, units='km')
# ggplotboatCcirc, aes(x=Longitude, y=Latitude)) + geom_point() +
#   geom_point(aes(x=center[2], y=center[1]), color='darkgreen', size=3)
# qplot(dists)

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

logTest <- function(vals=seq(-3,3, length.out=100), n) {
  log(dnorm(vals, 0, 1)^n)
}
vals <- seq(-3,3, length.out=100)
logDat <- data.frame(x=vals) %>%
  mutate(log1 = logTest(x, 1),
         log2 = logTest(x, 2))

ggplot(logDat, aes(x=x)) + geom_point(aes(y=log1), color='red') +
  geom_point(aes(y=log2), color='blue') +
  geom_point(aes(y=log2/2), color='green')



