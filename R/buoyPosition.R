#' @name buoyPosition
#' @title Buoy Position
#' @description Return estimated position of buoys given a set of times
#'
#' @param x a data frame containing a column specifying buoy (\code{Buoy}) and
#'   time (\code{UTC})
#' @param buoy.data data.frame of buoy positions. Format depends on choice of
#'   \code{method}
#' @param method method of position estimation to use
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @export
#'
buoyPosition <- function(x, buoy.data, method = c("initial", "interpolate", "drift"), ...) {
  switch(
    match.arg(method),
    initial = buoyPositionInitial(x, buoy.data)
  )
}

#' @rdname buoyPosition
#'
buoyPositionInitial <- function(x, buoy.data) {
  do.call(
    rbind,
    lapply(
      1:nrow(x),
      function(i) buoy.data[[x$Buoy[i]]][1, c("Latitude", "Longitude")]
    )
  )
}
