#' @name mapStations
#' @title Create a Summary Map of All Sonobuoy Stations
#' @description Return a ggmap object showing all sonobuoy stations.
#'
#' @param stationList list of sonobuoy stations returned by loadStations
#' @param zoom zoom level used for the map. Default is 'auto', will back out from
#'   a zoom level of 10 until all data fits. If an integer, will force that zoom
#'   level to be used.
#' @param crop flag whether or not to automatically crop the map to range of your data
#' @param map Optional, a ggmap object to plot on. If left as NULL, will be created automatically
#'   by the getMap function. Can be included to reduce calls to getMap.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import ggplot2
#' @export
#'
mapStations <- function(stationList, zoom='auto', crop=FALSE, map=NULL) {
  buoyPositions <- do.call(rbind, lapply(stationList, function(s) {
    do.call(rbind, lapply(s$buoys, function(b) {
      buoyDat <- b$position[1,]
      buoyDat$Station <- gsub('(.*)\\..*', '\\1', attr(s, 'station'))
      buoyDat
    }))
  }))

  # Checking if we are crossing the dateline
  buoyPositions <- fixDateline(buoyPositions)
  boundLong <- range(buoyPositions$Longitude)
  boundLat <- range(buoyPositions$Latitude)

  if(is.null(map)) {
    if(zoom=='auto') {
      map <- getMap(buoyPositions, force=FALSE)
    } else {
      map <- getMap(buoyPositions, force=TRUE, zoom=round(zoom))
    }
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
