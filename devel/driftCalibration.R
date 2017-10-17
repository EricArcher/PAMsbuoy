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
driftCalibration <- function(buoy.data, graph=FALSE, initial = c(1, 0)) {
  lapply(buoy.data, function(buoy) {
    start <- buoy$position[1,]
    if(!graph) {
      drift <- optim(par=initial, negLogl, boat=buoy$calibration, start=start, hessian=TRUE,
                     method='L-BFGS-B', lower=c(-3, -360), upper=c(3, 360))
    }
    list(rate=drift$par[1], bearing=drift$par[2], hessian=drift$hessian)
  })
}

negLogl <- function(boat, start, drift) {
  expected <- expectedBearing(boat, start, drift[1], drift[2])
  error <- sapply((boat$DIFARBearing - expected) %% 360, function(x) {
    if(x < abs(x-360)) {x}
    else {x-360}
  }
  )
  (nrow(boat)/2)*log(2*pi*(4^2)) + (1/2/(4^2))*sum((error)^2)
}

expectedBearing <- function(boat, start, drift.rate, drift.phi) {
  drift.distance <- drift.rate*difftime(boat$UTC, start$UTC, units='secs')/3600 # using seconds
  buoyLoc <- matrix(
    destination(start$Latitude, start$Longitude, brng=drift.phi, distance=drift.distance, units='km'),
    ncol=2)
  swfscMisc::bearing(buoyLoc[,1], buoyLoc[,2], boat$BoatLatitude, boat$BoatLongitude)
}