speciesMap <- function(stationList, species, ...) {
  speciesData <- do.call(rbind, lapply(stationList, function(s) {
    specs <- s$detections %>% mutate(Station = gsub('(.*)\\..*', '\\1', attr(s, 'station')))
    cbind(specs, buoyPosition(specs, transpose(s$buoys)$position))
  }))
  rownames(speciesData) <- c()
  specList <- unique(speciesData$Species)
  buoyList <- unique(speciesData$Buoy)
}



new <- do.call(rbind, lapply(calCur, function(s) {
  specs <- s$detections %>%
    mutate(Station = gsub('(.*)\\..*', '\\1', attr(s, 'station')))
  if(nrow(specs) > 0) {
    specs <- cbind(specs, buoyPosition(specs, transpose(s$buoys)$position))
  }
  specs}))
rownames(new) <- c()
specList <- unique(new$Species)
buoyList <- unique(new$Buoy)

# This isn't done by station at all, but seems to work. Need to think.
test <- sapply(specList, function(s) {
  sapply(buoyList, function(b) {
    sum((new$Species==s) & (new$Buoy==b))})
})

