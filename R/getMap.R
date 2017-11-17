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
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom ggmap get_map ggmap
#' @export
#'
getMap <- function(positions, force=FALSE, zoom=10) {
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
  boundLong <- range(positions$Longitude)
  boundLat <- range(positions$Latitude)
  suppressMessages(map <- get_map(location = c(lon=mean(boundLong), lat=mean(boundLat)), zoom=zoom))

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
