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
driftCalibration <- function(buoy.data, graph=FALSE, initial=c(1, 0), ...) {
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
      drift <- optim(par=initial, driftLogl, boat=buoy$calibration, start=start,
                     control=list('fnscale'=-1, maxit=10000, parscale=c(30,1)),
                     hessian=TRUE, method='L-BFGS-B', lower=c(0, 0), upper=c(3, 360))
    }
    list(rate=drift$par[1], bearing=drift$par[2], hessian=drift$hessian)
  })
}

driftLogl <- function(boat, start, drift, sd=4) {
  expected <- expectedBearing(boat, start, drift[1], drift[2])
  error <- sapply((boat$DIFARBearing - expected) %% 360, function(x) {
    if(x < abs(x-360)) {x}
    else {x-360}
  }
  )
  -1*(nrow(boat)/2)*log(2*pi*(sd^2)) - (1/2/(sd^2))*sum((error)^2)
}

expectedBearing <- function(boat, start, drift.rate, drift.phi) {
  drift.distance <- sapply(boat$UTC, function(t) {
    drift.rate*difftime(t, start$UTC, units='secs')/3600 # using seconds
  })
  buoyLoc <- matrix(
    swfscMisc::destination(start$Latitude, start$Longitude, brng=drift.phi, distance=drift.distance, units='km'),
    ncol=2)
  swfscMisc::bearing(buoyLoc[,1], buoyLoc[,2], boat$BoatLatitude, boat$BoatLongitude)[1:nrow(boat)]
}

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