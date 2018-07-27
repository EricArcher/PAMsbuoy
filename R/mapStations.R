#' @name mapStations
#' @title Create a Summary Map of All Sonobuoy Stations
#' @description Return a ggmap object showing all sonobuoy stations.
#'
#' @param detectionData detection summary data returned by \code{detectionSummary}
#' @param zoom zoom level used for the map. Default is 'auto', will back out from
#'   a zoom level of 10 until all data fits. If an integer, will force that zoom
#'   level to be used.
#' @param crop flag whether or not to automatically crop the map to range of your data
#' @param map Optional, a ggmap object to plot on. If left as NULL, will be created automatically
#'   by the getMap function. Can be included to reduce calls to getMap.
#' @param title a title for the plot
#' @param colorBy column to color points by
#' @param size size of points to plot
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import ggplot2
#' @export
#'
mapStations <- function(detectionData, zoom='auto', crop=FALSE, map=NULL,
                        title='Sonobuoy Stations', colorBy = 'cruise', size=3) {
  # buoyPositions <- do.call(rbind, lapply(stationList, function(s) {
  #   do.call(rbind, lapply(s$buoys, function(b) {
  #     buoyDat <- b$position[1,]
  #     buoyDat$Station <- gsub('(.*)\\..*', '\\1', attr(s, 'station'))
  #     buoyDat
  #   })) %>% mutate(Cruise = s$stationInfo$cruise)
  # }))

  # Checking if we are crossing the dateline
  detectionData <- fixDateline(detectionData)
  boundLong <- range(detectionData$Longitude)
  boundLat <- range(detectionData$Latitude)

  if(is.null(map)) {
    if(zoom=='auto') {
      map <- getMap(detectionData, force=FALSE)
    } else {
      map <- getMap(detectionData, force=TRUE, zoom=round(zoom))
    }
  }
  colorPalette <- c("#000000","#009E73", "#0072B2", "#D55E00", "#F0E442", "#CC79A7")

  # Colorblind palette. Just in case adding color by factor later.
  # cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  g <- map + geom_point(data=detectionData, aes_string(x='Longitude', y='Latitude', color=colorBy), size=size) +
    labs(x='Longitude', y='Latitude', title=title, color='') +
    # scale_color_manual(values=colorPalette) +
    theme(plot.title = element_text(hjust=.5))
  # Use CB and blue friendly palette up to a certain number of colors, if more then whatever
  if(length(unique(detectionData[[colorBy]])) <= length(colorPalette)) {
    g <- g + scale_color_manual(values=colorPalette)
  }

  # Re-doing labels in case we have crossed dateline, ie -190 -> +170
  longLabsOld <- as.numeric(ggplot_build(g)$layout$panel_params[[1]]$x.labels)
  longLabsNew <- sapply(longLabsOld, function(x) (x-180) %% 360 - 180)

  suppressMessages(g <- g + scale_x_continuous(breaks=longLabsOld, labels=longLabsNew, expand=c(0,0)))

  if(crop) {
    suppressMessages(g <- g + xlim(boundLong) + ylim(boundLat))
  }
  g
}
