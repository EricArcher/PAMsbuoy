#' @name getMap
#' @title Get a Map of Appropriate Size
#' @description Return a ggmap object centered on the middle of the positions
#'    data. Will start zoomed in then gradually zoom out until all points fit
#'    unless force is TRUE
#'
#' @param positions data frame containing columns \code{Latitude} and \code{Longitude}
#' @param zoom zoom level used for the map. An integer, see zoom description in
#'    get_map function of ggmap
#' @param force flag whether or not to force a specific zoom level instead of
#'   finding it automatically
#' @param center the location to center the map on. If \code{NULL}, will use the
#'   mean of the range of the data. If not \code{NULL} it must be a named vector
#'   with values \code{lon} and \code{lat}
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom ggmap get_map ggmap
#' @export
#'
getMap <- function(positions, force=FALSE, zoom=10, center=NULL) {
  # We cant automatically map near the poles yet, just stop for now.
  poleThresh <- 70
  if(max(abs(positions$Latitude)) > poleThresh) {
    stop('It looks like you are near one of the poles. Automatic mapping not yet supported here. Sorry!')
  }
  # If we tried to zoome out this far something bad happened.
  if(zoom<2) {
    stop('Cannot use Zoom 0 or 1. Check coordinates for errors.')
  }
  # Get our map with smallest bounding box
  positions <- fixDateline(positions)
  boundLong <- range(positions$Longitude)
  boundLat <- range(positions$Latitude)
  if(is.null(center)) {
    center <- c(lon=mean(boundLong), lat=mean(boundLat))
  }
  # Try downloading up to three times - fails quite often, but will work on second try
  nTries <- 3
  map <- NULL
  suppressMessages(
    for(t in 1:nTries) {
      try(
        map <- get_map(location = center, zoom=zoom)
      )
      if(!is.null(map)) break
    }
  )

  # Checking if all points are within map range. If not, zoom out 1.
  mapRange <- attr(map, 'bb')
  if(!force & (
    boundLong[1] < mapRange[2] |
    boundLong[2] > mapRange[4] |
    boundLat[1] < mapRange[1] |
    boundLat[2] > mapRange[3])) {
    # statement mostly useful for debugging
    # cat('Zoom level', zoom, 'is too close. Trying', zoom-1,'. \n')
    return(getMap(positions, force, zoom-1))
  }
  cat('Zoom level', zoom, 'being used.')
  ggmap(map)
}

