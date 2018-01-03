# Drift Simulated Testing
library(ggplot2)
library(dplyr)

makeCircle <- function(center, distance, shape = 0, boatKnots = 10,
                       angles=seq(from=0, to=360, length.out=360)) {
  c <- distance*shape
  dist <- sapply(angles, function(theta) {
    (-distance^2 + c^2)/(-distance+c*cos((90-theta)*pi/180))
  })
  points <- do.call(rbind, lapply(seq_along(angles), function(a) {
    start <- swfscMisc::destination(center[1], center[2], 270, c, units='km')
    swfscMisc::destination(start[1], start[2], a, dist[a], units='km')
  }))
  data.frame(BoatLatitude = points[,1], BoatLongitude = points[,2], Distance = dist,
             TimeDiff = cumsum(dist*(angles[2]-angles[1])/boatKnots/1.85*3600*pi/180))
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
lines <- makeLines(start=start, distances=c(1,2), boatKnots=10, angle=0,turn=135,nPoints=20)
buoy <- driftBuoy(start, rate=1, bearing=90, times=lines$UTC)
lines$Buoy <- 0
lines$DIFARBearing <- mapply(swfscMisc::bearing, buoy$Latitude, buoy$Longitude,
                             lines$BoatLatitude, lines$BoatLongitude)[1,]

driftCalibration(list(position=start, calibration=lines))

testit <- function(rate, bearing) {
  buoy <- driftBuoy(start, rate, bearing, times=lines$UTC)
  lines$Buoy <- 0
  lines$DIFARBearing <- mapply(swfscMisc::bearing, buoy$Latitude, buoy$Longitude,
                               lines$BoatLatitude, lines$BoatLongitude)[1,]
  drift <- driftCalibration(list(position=start, calibration=lines))[[1]]
  end <- swfscMisc::destination(start$Latitude, start$Longitude, drift$bearing,
                                drift$rate*difftime(lines$UTC[nrow(lines)], start$UTC, units='secs')/3600,
                                units='km')
  print(ggplot() + geom_point(data=lines, aes(x=BoatLongitude, y=BoatLatitude, color='Boat')) +
          geom_point(data=buoy, aes(x=Longitude, y=Latitude, color='Buoy')) +
          geom_segment(aes(x=start$Longitude, xend=end[2], y=start$Latitude, yend=end[1], color='Drift'), size=2, alpha=.4))
  drift
}
testit(3,45)

ggplot() + geom_point(data=lines, aes(x=BoatLongitude, y=BoatLatitude, color='Boat')) +
  geom_point(data=buoy, aes(x=Longitude, y=Latitude, color='Buoy'))

circ <- makeCircle(center=center, distance=distance, shape=shape, boatKnots=boatKnots, angles=angles)
# dists <- mapply(swfscMisc::distance, center[1], center[2], circ$Latitude, circ$Longitude, units='km')
# ggplot(circ, aes(x=Longitude, y=Latitude)) + geom_point() +
#   geom_point(aes(x=center[2], y=center[1]), color='darkgreen', size=3)
# qplot(dists)



