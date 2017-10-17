library(geosphere)
library(dplyr)
library(lubridate)
library(ggplot2)
library(swfscMisc)
library(plotly)
library(viridisLite)
library(R.utils)
library(tidyr)
# source('~/R Projects/SWFSC/PAMsbuoy/devel/drawBearing.R')
source('./devel/drawBearing.R')
source('../SonoBuoy/driftFunctions.R')
sourceDirectory('./R/')

# Test using package
# pamDb <- loadDB('../PAMsbuoy/devel/final db formatting/FinalFormat_Station1.sqlite3')
load('../SonoBuoy/pamDb.Rdata')
pamDifar <- formatStation(pamDb)

# estimatePosition(time, db$gpsData) gives gps at times and mag var

# Might need to do drit in the calculateOffset function. This is where we get true.bearing, so it has the
# lat long from gps. Or we just have that keep the lat long so we can use it later.
# The shit I want is in buoy$number$calibration
buoyToDo <- pamDifar$buoys$`3`
buoyToDo$position[1,]
buoyToDo$calibration

lapply(pamDifar$buoys, function(b) {
  start <- b$position[1,]
  drift <- calculateDrift(start=start, difar=b$calibration)
  list(rate=drift$par[1], bearing=drift$par[2], hessian=drift$hessian)
})

drift <- calculateDrift(start=buoyToDo$position[1,], difar=buoyToDo$calibration)
drift$par
sqrt(solve(drift$hessian))
# drift testing area

calculateDrift <- function(start, difar, niter = 10000, initial = c(1, 0), graph=NULL) {
  if(is.null(graph)) {
    drift <- optim(par=initial, negLogl, boat=difar, start=start, hessian=TRUE,
                   method='L-BFGS-B', lower=c(-3, -360), upper=c(3, 360))
  }
  drift
}

driftGrid <- function(values=list(rate=seq(0,2, length.out=75), phi=seq(0,360,2)), fun, boat, start) {
  # df <- data.frame(rate = unlist(lapply(values$rate, function(x) rep(x, length(values$phi)))),
  #                  phi = rep(values$phi, length(values$rate)))
  # df$value <- mapply(fun, df$rate, df$phi, MoreArgs=list(boat=boat, start=start))
  # df
  do.call(rbind, lapply(values$rate, function(x) {
    value <- sapply(values$phi, function(y) {
      fun(boat, start, c(x, y))
    })
    data.frame(rate=x, phi=values$phi, value=value)
  }))
}