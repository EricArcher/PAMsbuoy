#' @name mapStations
#' @title Create a Summary Map of All Sonobuoy Stations
#' @description Return a ggmap object showing all sonobuoy stations.
#'
#' @param stationList list of sonobuoy stations returned by loadStations
#' @param zoom zoom level used for the map
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom ggmap get_map ggmap
#' @import ggplot2
#' @export
#'
mapStations <- function(stationList, zoom=8, force=FALSE) {
  buoyPositions <- do.call(rbind, lapply(stationList, function(s) {
    do.call(rbind, lapply(s$buoys, function(b) {
      buoyDat <- b$position[1,]
      buoyDat$Station <- gsub('(.*)\\..*', '\\1', attr(s, 'station'))
      buoyDat
    }))
  }))
  makeMap(buoyPositions, zoom, force)
}

# Needs some checking for dateline. Right now will blow up.
makeMap <- function(buoyPositions, zoom=8, force=FALSE, ...) {
  # Date line will only use negative values. Not sure about this.
  buoyPositions$Longitude <- (buoyPositions$Longitude %% 360) - 360
  boundLong <- range(buoyPositions$Longitude)
  boundLat <- range(buoyPositions$Latitude)
  if(zoom==0) {
    stop('Zoom is 0. Check coordinates for errors.')
  }
  suppressMessages(map <- get_map(location = c(lon=mean(boundLong), lat=mean(boundLat)), zoom=zoom))
  mapRange <- attr(map, 'bb')
  # Checking if all points are within map range. If not, zoom out 1.
  if(!force & (
    boundLong[1] < mapRange[2] |
     boundLong[2] > mapRange[4] |
     boundLat[1] < mapRange[1] |
     boundLat[2] > mapRange[3])) {
    cat('Zoom level', zoom, 'is too close. Trying', zoom-1,'. \n')
    return(makeMap(buoyPositions, zoom-1, force))
  }
  # Colorblind colors
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  cat('Zoom level', zoom, 'being used.')
  g <- ggmap(map) + geom_point(data=buoyPositions, aes(x=Longitude, y=Latitude, color='Station'), size=3, ...) +
    labs(x='Longitude', y='Latitude', title='Sonobuoy Stations', color='') +
    scale_color_manual(values=cbPalette) +
    theme(plot.title = element_text(hjust=.5))
  g
}
