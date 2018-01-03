# Drift Simulated Testing

makeCircle <- function(center, distance, shape = 0,
                       angles=seq(from=0, to=360, length.out=360)) {
  c <- distance*shape
  dist <- sapply(angles, function(theta) {
    (-distance^2 + c^2)/(-distance+c*cos((90-theta)*pi/180))
  })
  points <- do.call(rbind, lapply(seq_along(angles), function(a) {
    start <- swfscMisc::destination(center[1], center[2], 270, c, units='km')
    swfscMisc::destination(start[1], start[2], a, dist[a], units='km')
  }))
  data.frame(Latitude = points[,1], Longitude = points[,2], Distance = dist)
}

# center <- c(32, -117); distance <- 1.5; shape=0; angles=seq(from=0, to=360, length.out=360)
#
# circ <- makeCircle(center, distance, shape, angles)
# dists <- mapply(swfscMisc::distance, center[1], center[2], circ$Latitude, circ$Longitude, units='km')
# ggplot(circ, aes(x=Longitude, y=Latitude)) + geom_point() +
#   geom_point(aes(x=center[2], y=center[1]), color='darkgreen', size=3)
# qplot(dists)
