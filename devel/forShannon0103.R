# Shit for Shannon
# 1
stationMap <- mapStations(calStations) + labs(title='CalCurCEAS Sonobuoy Stations')

calSum <- detectionSummary(calStations)
calMap <- getMap(calSum)
allDetMap <- mapDetections(calSum, map=calMap)
splitDetMap <- mapDetections(calSum, map=calMap, combine=FALSE)

# 2
db <- loadDB('../SonoBuoy/Data/PAST_20160607_POST_VesselCalOnly.sqlite3')
shanBuoy <- read.csv('../SonoBuoy/Data/Buoy0607.csv')
shanStation <- formatStation(db, buoyPositions = shanBuoy)
shanStation$buoys$`1`$calibration <- filter(shanStation$buoys$`1`$calibration, abs(offset) < 10)
checkCalibrations(shanStation, recheck=TRUE)

checkCalibrations(calStations)
checkCalibrations(setteStations)

first <- do.call(rbind,lapply(split(db$DIFAR_Localisation, db$DIFAR_Localisation$Channel), function(x) {
  head(arrange(x, UTC),1)
}))

db$HydrophoneStreamers %>% select(StreamerIndex, UTC, Latitude, Longitude) %>%
  arrange(StreamerIndex, UTC) %>% filter(StreamerIndex==1)
