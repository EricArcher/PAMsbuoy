#' @name buoyPosition
#' @title Buoy Position
#' @description Return estimated position of buoys given a set of times
#'
#' @param x a data frame containing a column specifying buoy (\code{Buoy}) and
#'   time (\code{UTC})
#' @param buoyData data.frame of buoy positions. Format depends on choice of
#'   \code{method}
#' @param method method of position estimation to use. \code{initial} chooses the
#'   earliest position, \code{interpolate} will interpolate within multiple
#'   positions (e.g., from GPS), and \code{drift} uses a drift model
#' @param \dots arguments passed on to other functions
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @export
#'
buoyPosition <- function(x, buoyData, method = c("initial", "interpolate", "drift"), ...) {
  switch(
    match.arg(method),
    initial = buoyPositionInitial(x, buoyData)
  )
}

#' @rdname buoyPosition
#'
buoyPositionInitial <- function(x, buoyData) {
  do.call(
    rbind,
    lapply(
      1:nrow(x),
      function(i) buoyData[[x$Buoy[i]]][1, c("Latitude", "Longitude")]
    )
  )
}
