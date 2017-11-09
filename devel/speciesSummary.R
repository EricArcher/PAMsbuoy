#' @name speciesSummary
#' @title Create a Summary Dataframe of All Species Detections
#' @description Return a dataframe containing the counts of all detected
#'    species at each station
#'
#' @param stationList list of sonobuoy stations returned by loadStations
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import dplyr
#' @importFrom tidyr gather
#' @export
#'
speciesSummary <- function(stationList) {
  # Will base points on buoy position info
  buoyPositions <- do.call(rbind, lapply(stationList, function(s) {
    do.call(rbind, lapply(s$buoys, function(b) {
      buoyDat <- b$position[1,]
      buoyDat$Station <- gsub('(.*)\\..*', '\\1', attr(s, 'station'))
      buoyDat
    }))
  }))
  # Then match it to detections. This is so we can easily identify 0 detection scenarios.
  speciesData <- do.call(rbind, lapply(stationList, function(s) {
    specs <- s$detections %>% mutate(Station = gsub('(.*)\\..*', '\\1', attr(s, 'station')))
    specs
  }))
  rownames(speciesData) <- c()
  # Identify all species we have. Might want to adjust this to try and match calls or whatever
  specList <- unique(speciesData$Species)
  specSummary <- do.call(rbind, by(buoyPositions, buoyPositions$Station, function(st) {
    for(sp in specList) {
      for(i in 1:nrow(st)) {
        st[i, sp] <- filter(speciesData, Species == sp,
                            Buoy == st$Buoy[i],
                            Station == st$Station[i]) %>% nrow()
      }
    }
    st}))
  rownames(specSummary) <- c()
  gather(specSummary, Species, Count, !! specList)
}
