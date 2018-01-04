# Shit for Shannon
# 1
stationMap <- mapStations(calStations) + labs(title='CalCurCEAS Sonobuoy Stations')

calSum <- detectionSummary(calStations)
calMap <- getMap(calSum)
allDetMap <- mapDetections(calSum, map=calMap)
splitDetMap <- mapDetections(calSum, map=calMap, combine=FALSE)

# 2
checkCalibrations(calStations)
checkCalibrations(setteStations)
