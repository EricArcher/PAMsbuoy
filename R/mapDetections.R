#' @name mapDetections
#' @title Create a Map Showing Numbers of Detections
#' @description Return a ggmap object showing the number of detections at each sonobuoy
#'    station. Can show maps for all detections, or broken up by species.
#'
#' @param detectionData a dataframe with data on detections. Must have columns
#'    \code{Latitude}, \code{Longitude}, \code{Station}, and \code{Count}. Must
#'    also have column \code{Species} unless bySpecies is \code{'none'}
#' @param species The species to look at. Either a vector of specific species, or
#'    \code{'all'} to use all species.
#' @param combine Should the counts for the selected species be combined. If
#'    \code{FALSE}, graph will be faceted for each species.
#' @param value column name of values to plot
#' @param map Optional, a ggmap object to plot on. If left as NULL, will be created by the
#'    getMap function. Can be included to reduce calls to getMap.
#' @param size Size of points to be plotted
#' @param ncol number of columns when plotting multiple species
#' @param palette color palette to be used, see brewer.pal for more info
#' @param nGroups the number of different groups to use for coloring. Groups will be
#'    evenly spaced between 0 and max number of detections.
#' @param grouping The level to group count data by. Default is Station, can be set
#'    to \code{c('Station', 'Buoy')} to group at buoy level.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom RColorBrewer brewer.pal
#' @export
#'
mapDetections <- function(detectionData, species='all', combine=TRUE, value='NumDetections', map=NULL,
                          size=3, ncol=3, palette='Reds', nGroups=6, grouping='station') {
  detectionData$plotMe <- detectionData[[value]]
  detectionData <- if(all(species %in% 'all')) {
    detectionData
  } else if(all(species %in% unique(detectionData$Species))) {
    filter(detectionData, Species %in% species)
  } else {
    stop(paste('species argument ', paste(species, collapse=' '), 'is invalid.'))
  }

  mapData <- if(combine) {
    detectionData %>% group_by_(.dots=grouping) %>%
      summarise(Longitude=median(Longitude), Latitude=median(Latitude),
                Count=sum(plotMe)) %>% data.frame() %>%
      mutate(Species = paste(species, collapse=' '))
  } else {
    detectionData %>% group_by_(.dots=c(grouping, 'Species')) %>%
      summarise(Longitude=median(Longitude), Latitude=median(Latitude),
                Count=max(plotMe)) %>% data.frame()
  }
  # Break into groups for coloring, then re-label groups for happiness.
  # sd <- round(sd(mapData$Count))
  # breaks <- seq(0, max(mapData$Count)+sd, sd)
  breaks <- seq(0, max(mapData$Count), length.out=nGroups+1)
  breaks[1] <- 1
  breaks <- round(c(0, breaks))
  breaks[length(breaks)] <- min(breaks[length(breaks)], max(mapData$Count))
  breaks <- unique(breaks)
  mapData$Breaks <- cut(mapData$Count, breaks, ordered_result = TRUE, include.lowest = TRUE, right=FALSE, dig.lab=4)
  haveLevels <- sort(as.numeric(unique(mapData$Breaks)))
  haveLabels <- c('0', levels(mapData$Breaks)[-1])[haveLevels]
  haveLabels <- gsub('(\\[|\\]|\\(|\\))', '', haveLabels)
  haveLabels <- gsub(',', ' to ', haveLabels)
  myPalette <- c('red', brewer.pal(nGroups, palette))
  usePalette <- myPalette[haveLevels]

  # Checking if we are crossing the dateline.
  mapData <- fixDateline(mapData)

  # Get map if needed
  if(is.null(map)) {
    detectionMap <- getMap(mapData)
  } else {
    detectionMap <- map
  }

  g <- detectionMap + geom_point(data=mapData, aes(x=Longitude, y=Latitude, color=Breaks, shape=Count==0), size=size) +
    facet_wrap(~Species, ncol=ncol) +
    scale_color_manual(values=usePalette, labels=haveLabels) +
    scale_shape_manual(values=c('FALSE'=16, 'TRUE'=4), guide=FALSE) +
    labs(x='Longitude', y='Latitude', color='Detections') +
    guides(color=guide_legend(override.aes = list(shape=ifelse(haveLabels=='0', 4, 16)))) +
    # guides(color=guide_legend(override.aes = list(shape=c(4, rep(16, length(haveLevels)-1))))) +
    theme(legend.key = element_rect(fill='#A3CCFF'))

  # Re-doing labels in case we have crossed dateline, ie -190 -> +170
  longLabsOld <- as.numeric(ggplot_build(g)$layout$panel_params[[1]]$x.labels)
  longLabsNew <- sapply(longLabsOld, function(x) (x-180) %% 360 - 180)

  suppressMessages(g <- g + scale_x_continuous(breaks=longLabsOld, labels=longLabsNew, expand=c(0,0)))
  g
}