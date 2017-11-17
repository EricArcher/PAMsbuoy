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
#' @param nrow number of rows when plotting multiple species
#' @param palette color palette to be used, see brewer.pal for more info
#' @param nGroups the number of different groups to use for coloring. Groups will be
#'    evenly spaced between 0 and max number of detections.
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom RColorBrewer brewer.pal
#' @export
#'
mapDetections <- function(detectionData, species='all', combine=TRUE, value='Count', map=NULL,
                          size=3, nrow=1, palette='Reds', nGroups=6) {
  if(is.null(map)) {
    detectionMap <- getMap(detectionData)
  } else {
    detectionMap <- map
  }

  detectionData$PlotMe <- detectionData[[value]]

  detectionData <- if(all(species %in% 'all')) {
    detectionData
  } else if(all(species %in% unique(detectionData$Species))) {
    filter(detectionData, Species %in% species)
  } else {
    stop(paste('species argument ', paste(species, collapse=' '), 'is invalid.'))
  }

  mapData <- if(combine) {
    detectionData %>% group_by(Station) %>%
      summarise(Longitude=median(Longitude), Latitude=median(Latitude),
                Count=sum(PlotMe)) %>% data.frame() %>%
      mutate(Species = paste(species, collapse=' '))
  } else {
    detectionData %>% group_by(Station, Species) %>%
      summarise(Longitude=median(Longitude), Latitude=median(Latitude),
                Count=max(PlotMe)) %>% data.frame()
  }

  # mapData <- if(all(bySpecies %in% 'none')) {
  #   detectionData %>% group_by(Station) %>%
  #     summarise(Latitude = median(Latitude), Longitude = median(Longitude),
  #               Count = sum(PlotMe)) %>% data.frame() %>%
  #     mutate(Species = 'All Species')
  # } else if(all(bySpecies %in% 'all')) {
  #   detectionData %>% group_by(Station, Species) %>%
  #     summarise(Latitude = median(Latitude), Longitude = median(Longitude),
  #               Count = sum(PlotMe)) %>% data.frame()
  # } else if(all(bySpecies %in% unique(detectionData$Species))) {
  #   detectionData %>% group_by(Station, Species) %>%
  #     summarise(Latitude = median(Latitude), Longitude = median(Longitude),
  #               Count = sum(PlotMe)) %>% data.frame() %>%
  #     filter(Species %in% bySpecies)
  # } else {
  #   stop(paste('bySpecies argument', paste(bySpecies, collapse=' '), 'is not valid'))
  # }

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

  detectionMap + geom_point(data=mapData, aes(x=Longitude, y=Latitude, color=Breaks, shape=Count==0), size=size) +
    facet_wrap(~Species, nrow=nrow) +
    scale_color_manual(values=usePalette, labels=haveLabels) +
    scale_shape_manual(values=c(16, 4), guide=FALSE) +
    labs(x='Longitude', y='Latitude', color='Detections') +
    guides(color=guide_legend(override.aes = list(shape=c(4, rep(16, length(haveLevels)-1))))) +
    theme(legend.key = element_rect(fill='#A3CCFF'))
}