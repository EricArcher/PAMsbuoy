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
    if(graph) {
      driftDf <- likeDf(nAngles=30,nRates=30, boat=buoy$calibration, start=start, sd=sd) %>%
        arrange(desc(Value))
      graph <- ggplot(driftDf, aes(x=Angle, y=Rate, fill=log(-Value))) + geom_tile() +
        scale_fill_gradientn(colors=viridis(256, direction=-1, option='magma'))
      list(rate=driftDf$Rate[1], bearing=driftDf$Angle[1], graph=graph)

    } else {
      # initDrift <- likeDf(nAngles=10, nRates=10, boat=buoy$calibration, start=start, sd=sd) %>%
      #   arrange(desc(Value))
      # initial=c(initDrift$Rate[1], initDrift$Angle[1])
      drift <- optim(par=initial, driftLogl, boat=buoy$calibration, start=start, sd=sd,
                     control=list('fnscale'=-1, maxit=10000, parscale=c(30,1)),
                     hessian=TRUE, method='L-BFGS-B', lower=c(0, 0), upper=c(3, 360))
      list(rate=drift$par[1], bearing=drift$par[2], err=sqrt(diag(solve(-drift$hessian))))
    }
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
  # drift.distance <- sapply(boat$UTC, function(t) {
  #   drift.rate*difftime(t, start$UTC, units='secs')/3600 # using seconds
  # })
  drift.distance <- drift.rate*(as.numeric(boat$UTC)-as.numeric(start$UTC))/3600
  buoyLoc <- matrix(
    swfscMisc::destination(start$Latitude, start$Longitude, brng=drift.phi, distance=drift.distance, units='km'),
    ncol=2)
  swfscMisc::bearing(buoyLoc[,1], buoyLoc[,2], boat$BoatLatitude, boat$BoatLongitude)[1:nrow(boat)]
  # buoyLoc <- geosphere::destPoint(c(start$Longitude, start$Latitude), b=drift.phi, d=drift.distance*1000)
  # geosphere::bearing(buoyLoc, cbind(boat$BoatLongitude, boat$BoatLatitude))
}

likeDf <- function(nAngles=60, nRates=30, FUN=driftLogl, boat, start, sd=10) {
  angles <- seq(0,360, length.out=nAngles)
  rates <- seq(0, 3, length.out=nRates)
  do.call(rbind, lapply(rates, function(r) {
    value <- sapply(angles, function(a) {
      driftLogl(boat, start, c(r,a), sd)
    })
    data.frame(Rate=r, Angle=angles, Value=value)
  }))
}

