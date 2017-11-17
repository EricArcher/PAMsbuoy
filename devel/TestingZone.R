# Testing zone.

db <- loadDB('../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/1647_SB_S102S103s.sqlite3')

station <- formatStation(db)

# Calcurceas
calStations <- loadStations('../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/', extraCols='TrackedGroup')
# Sette
setteStations <- loadStations('../SonoBuoy/Data/HICEAS_2017/Sette/Database/')
# Lasker
laskerStations <- loadStations('../SonoBuoy/Data/HICEAS_2017/Lasker/Database/')

# All the P1 P2 shit is messed up. Gotta combine all janky like
calSum <- detectionSummary(calStations) %>%
  mutate(Station = gsub('_P[1-9]$', '', Station)) %>%
  group_by(Buoy, Station, Species) %>%
  summarise(Latitude = median(Latitude), Longitude = median(Longitude),
            UTC = median(UTC), Count = sum(Count), UniqueCount = sum(UniqueCount)) %>%
  data.frame()

map <- getMap(calSum)
mapDetections(calSum, map=map, palette = 'YlOrRd', combine=F, nGroups = 9, species=c('bma', 'bmb'))

mapStations(setteStations)
