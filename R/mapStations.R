#' @name mapStations
#' @title Create a Summary Map of All Sonobuoy Stations
#' @description Return a ggmap object showing all sonobuoy stations.
#'
#' @param stationList list of sonobuoy stations returned by loadStations
#' @param zoom zoom level used for the map. Default is 'auto', will back out from
#'   a zoom level of 10 until all data fits. If an integer, will force that zoom
#'   level to be used.
#' @param crop flag whether or not to automatically crop the map to range of your data
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import ggplot2
#' @export
#'
mapStations <- function(stationList, zoom='auto', crop=FALSE) {
  buoyPositions <- do.call(rbind, lapply(stationList, function(s) {
    do.call(rbind, lapply(s$buoys, function(b) {
      buoyDat <- b$position[1,]
      buoyDat$Station <- gsub('(.*)\\..*', '\\1', attr(s, 'station'))
      buoyDat
    }))
  }))
  boundLong <- range(buoyPositions$Longitude)
  boundLat <- range(buoyPositions$Latitude)
  # Checking if we are crossing the dateline. When grabbing map later the coordinates will be
  # positive or negative based on the center point used, so we check the sign of center and
  # adjust accordingly. This needs to change if we want option to force center spot.
  if((boundLong[2]-boundLong[1])>180) {
    if((boundLong[1]+boundLong[2]) < 0) {
      buoyPositions$Longitude <- (buoyPositions$Longitude %% 360)
    } else {
      buoyPositions$Longitude <- (buoyPositions$Longitude %% 360) - 360
    }
    boundLong <- range(buoyPositions$Longitude)
  }
  if(zoom=='auto') {
    map <- getMap(buoyPositions, force=FALSE)
  } else {
    map <- getMap(buoyPositions, force=TRUE, zoom=round(zoom))
  }

  # Colorblind palette. Just in case adding color by factor later.
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  g <- map + geom_point(data=buoyPositions, aes(x=Longitude, y=Latitude, color='Station'), size=3) +
    labs(x='Longitude', y='Latitude', title='Sonobuoy Stations', color='') +
    scale_color_manual(values=cbPalette) +
    theme(plot.title = element_text(hjust=.5))

  # Re-doing labels in case we have crossed dateline, ie -190 -> +170
  longLabsOld <- as.numeric(ggplot_build(g)$layout$panel_ranges[[1]]$x.labels)
  longLabsNew <- sapply(longLabsOld, function(x) (x-180) %% 360 - 180)

  suppressMessages(g <- g + scale_x_continuous(breaks=longLabsOld, labels=longLabsNew, expand=c(0,0)))

  if(crop) {
    suppressMessages(g <- g + xlim(boundLong) + ylim(boundLat))
  }
  g
}