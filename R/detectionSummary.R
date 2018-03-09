#' @name detectionSummary
#' @title Create a Summary Dataframe of All Species Detections
#' @description Return a dataframe containing the counts of all detected
#'    species at each station
#'
#' @param stationList list of sonobuoy stations returned by loadStations
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import dplyr
#' @importFrom tidyr gather_
#' @export
#'
detectionSummary <- function(stationList) {
  # Will base points on buoy position info
  buoyPositions <- do.call(rbind, lapply(stationList, function(s) {
    do.call(rbind, lapply(s$buoys, function(b) {
      buoyDat <- b$position[1,]
      buoyDat$Station <- gsub('(.*)\\..*', '\\1', attr(s, 'station'))
      buoyDat
    })) %>%
      mutate(StationType = s$stationInfo$station_type,
             Cruise = s$stationInfo$cruise,
             SightingId = as.numeric(s$stationInfo$vis_id),
             RecordingLength = s$stationInfo$recordingLength)
  }))
  # Then match it to detections. This is so we can easily identify 0 detection scenarios.
  detectionData <- do.call(rbind, lapply(stationList, function(s) {
    dets <- s$detections %>% mutate(Station = gsub('(.*)\\..*', '\\1', attr(s, 'station')))
    dets
  }))
  rownames(detectionData) <- c()
  # Identify all species we have. Might want to adjust this to try and match calls or whatever
  specList <- unique(detectionData$Species)
  if(length(specList)==0) {
    specList <- 'NONE'
  }
  detSummary <- do.call(rbind, by(buoyPositions, buoyPositions$Station, function(st) {
    for(sp in specList) {
      for(i in 1:nrow(st)) {
        st[i, sp] <- filter(detectionData, Species == sp,
                            Buoy == st$Buoy[i],
                            Station == st$Station[i]) %>% nrow()
        # Getting unique calls using 'detection' column from formatting
        st[i, paste0('unique_', sp)] <- filter(detectionData, Species == sp,
                                               Station == st$Station[i]) %>% distinct(detection) %>% nrow()
      }
    }
    st}))
  rownames(detSummary) <- c()
  gather_(detSummary, 'Species', 'Count', specList) %>%
    gather_('USpecies', 'UniqueCount', paste0('unique_', specList)) %>%
    mutate_('USpecies' = ~ gsub('unique_', '', USpecies)) %>%
    filter_('USpecies==Species') %>% select_('-USpecies') %>%
    rename_(.dots = setNames(c('Count', 'UniqueCount'), c('NumDetections', 'UniqueDetections'))) %>%
    group_by(Station) %>% mutate(mn=min(UTC)) %>%
    arrange_(.dots = c('mn', 'Station', 'Species')) %>%
    ungroup() %>% select(-mn)
}
