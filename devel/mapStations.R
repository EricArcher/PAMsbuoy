#' @name mapStations
#' @title Create a Summary Map of All Sonobuoy Stations
#' @description Return a ggmap object showing all sonobuoy stations.
#'
#' @param stationList list of sonobuoy stations returned by formatStation
#' @param zoom zoom level used for the map
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom ggmap get_map ggmap
#' @import ggplot2
#' @export
#'
mapStations <- function(stationList, zoom=8) {
  buoyPositions <- do.call(rbind, lapply(stations, function(s) {
    do.call(rbind, lapply(s$buoys, function(b) {
      buoyDat <- b$position[1,]
      buoyDat$Station <- gsub('(.*)\\..*', '\\1', attr(stations, 'station'))
      buoyDat
    }))
  }))
  makeMap(buoyPositions, zoom)
}

# Needs some checking for dateline. Right now will blow up.
makeMap <- function(buoyPositions, zoom=8, ...) {
  boundLong <- range(buoyPositions$Longitude)
  boundLat <- range(buoyPositions$Latitude)
  if(zoom==0) {
    stop('Zoom is 0. Check coordinates for errors.')
  }
  suppressMessages(map <- get_map(location = c(lon=mean(boundLong), lat=mean(boundLat)), zoom=zoom))
  mapRange <- attr(map, 'bb')
  # Checking if all points are within map range. If not, zoom out 1.
  if(boundLong[1] < mapRange[2] |
     boundLong[2] > mapRange[4] |
     boundLat[1] < mapRange[1] |
     boundLat[2] > mapRange[3]) {
    cat('Zoom level', zoom, 'is too close. Trying', zoom-1,'. \n')
    return(makeMap(buoyPositions, zoom-1))
  }
  cat('Zoom level', zoom, 'being used.')
  g <- ggmap(map) + geom_point(data=buoyPositions, aes(x=Longitude, y=Latitude, color='Station'), size=3, ...) +
    labs(x='Longitude', y='Latitude', title='Sonobuoy Stations', color='') +
    scale_color_manual(values='black') +
    theme(plot.title = element_text(hjust=.5))
  g
}
