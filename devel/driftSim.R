# Drift Simulated Testing
library(ggplot2)
library(dplyr)
library(viridisLite)
library(plotly)

## Isnt working right with seq(0,360,length.out=180) check angles
makeCircle <- function(start, center, distance, shape = 0, boatKnots = 10,
                       angles=seq(from=0, to=360, length.out=360)) {
  c <- distance*shape
  dist <- sapply(angles, function(theta) {
    (-distance^2 + c^2)/(-distance+c*cos((90-theta)*pi/180))
  })
  points <- do.call(rbind, lapply(seq_along(angles), function(a) {
    start <- swfscMisc::destination(center[1], center[2], 270, c, units='km')
    swfscMisc::destination(start[1], start[2], a, dist[a], units='km')
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

center <- c(32, -117); distance <- 1.5; shape=0; angles=seq(from=0, to=360, length.out=360); boatKnots=10

start <- data.frame(Latitude=32, Longitude=-117, UTC=as.POSIXct('2017-08-08 08:00:00'), Buoy=0)

boatCircle <- makeCircle(start=start, center=start, distance=1.5,
                         angles=seq(from=0, to=360, length.out=360), boatKnots=10)
boatLines <- makeLines(start=start, distances=c(1,2), boatKnots=10, angle=0,turn=135,nPoints=20)

buoy <- driftBuoy(start, rate=2, bearing=45, times=boatLines$UTC)

makeDifar <- function(boat, buoy) {
  boat$Buoy <- 0
  boat$DIFARBearing <- mapply(swfscMisc::bearing, buoy$Latitude, buoy$Longitude,
                              boat$BoatLatitude, boat$BoatLongitude)[1,]
  boat
}
boatLines <- makeDifar(boatLines, start)


driftDf <- likeDf(nAngles=30,nRates=30, boat=boatLines, start=start) %>%
  arrange(desc(Value))
ggplot(driftDf, aes(x=Angle, y=Rate, color=Value)) + geom_point(size=8) +
  scale_color_gradientn(colors=viridis(256, direction=1, option='viridis'))

ggplot(driftDf, aes(x=Angle, y=Rate, z=Value, fill=Value)) + geom_tile() +
  scale_fill_gradientn(colors=viridis(256)) +
  coord_cartesian(xlim=c(0,360), ylim=c(0,3), expand=FALSE)

driftCalibration(list(position=start, calibration=boatLines))

testit <- function(boat, start, rate, bearing, plot=FALSE) {
  buoy <- driftBuoy(start, rate, bearing, times=boatLines$UTC)
  boat$Buoy <- 0
  boat$DIFARBearing <- mapply(swfscMisc::bearing, buoy$Latitude, buoy$Longitude,
                               boat$BoatLatitude, boat$BoatLongitude)[1,]
  drift <- driftCalibration(list(position=start, calibration=boat))[[1]]
  end <- swfscMisc::destination(start$Latitude, start$Longitude, drift$bearing,
                                drift$rate*difftime(boat$UTC[nrow(boat)], start$UTC, units='secs')/3600,
                                units='km')
  if(plot) {
    print(ggplot() + geom_point(data=boat, aes(x=BoatLongitude, y=BoatLatitude, color='Boat')) +
            geom_point(data=buoy, aes(x=Longitude, y=Latitude, color='Buoy')) +
            geom_segment(aes(x=start$Longitude, xend=end[2], y=start$Latitude, yend=end[1], color='Drift'),
                         size=2, alpha=.4))
  }
  drift
}

testit(boatLines, start, .5,45, plot=TRUE)
#
# ggplot() + geom_point(data=boatLines, aes(x=BoatLongitude, y=BoatLatitude, color='Boat')) +
#   geom_point(data=buoy, aes(x=Longitude, y=Latitude, color='Buoy'))
#
# boatCirc <- makeCircle(center=center, distance=distance, shape=shape, boatKnots=boatKnots, angles=angles)
# dists <- mapply(swfscMisc::distance, center[1], center[2], boatCirc$Latitude, boatCirc$Longitude, units='km')
# ggplotboatCcirc, aes(x=Longitude, y=Latitude)) + geom_point() +
#   geom_point(aes(x=center[2], y=center[1]), color='darkgreen', size=3)
# qplot(dists)

likeDf <- function(nAngles=60, nRates=30, FUN=driftLogl, boat, start) {
  angles <- seq(0,360, length.out=nAngles)
  rates <- seq(0, 3, length.out=nRates)
  do.call(rbind, lapply(rates, function(r) {
    value <- sapply(angles, function(a) {
      driftLogl(boat, start, c(r,a))
    })
    data.frame(Rate=r, Angle=angles, Value=value)
  }))
}

t <- do.call(rbind, lapply(driftDf$Rate, function(r) {
  value <- sapply(driftDf$Angle, function(a) {
    driftLogl(boatLines, start, c(r,a))
  })
  data.frame(Rate=r, Angle=driftDf$Angle, Value=value)
}))


