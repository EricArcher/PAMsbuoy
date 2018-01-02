# Testing zone.
library(PAMsbuoy)
library(dplyr)
library(tools)
library(ggplot2)
db <- loadDB('../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/1647_SB_S89S90s_P1.sqlite3')
db <- loadDB('../SonoBuoy/Data/PAST_20170620/PAST20Jun2017_pg11511_sbExperiment DIFAR - Circles.sqlite3')
db <- loadDB('../SonoBuoy/Data/PAST_20160607_POST_VesselCalOnly.sqlite3')
db <- loadDB('../SonoBuoy/Data/HICEAS_2017/Sette/Database/1706_pg11511_sb_10_20170722.sqlite3')
db <- loadDB('./devel/final db formatting/FinalFormat_Station2.sqlite3')
buoyPos <- data.frame(Buoy = '1', UTC='2014-08-08 03:19:27',
                      Latitude = 34.6, Longitude = -125)
station <- formatStation(db, override = F, dateFormat = '%m/%d/%Y %H:%M')

## Calcurceas
calPositions <-
  do.call(
    rbind,
    lapply(
      list_files_with_exts('../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite',
                           exts='sqlite3'), function(f) {
                             myDb <- loadDB(f)
                             myDb$HydrophoneStreamers %>%
                               mutate(Station = attr(myDb, 'station'), Buoy = StreamerIndex) %>%
                               select(UTC, Longitude, Latitude, Buoy, Station)
                           }))
# saveRDS(calPositions, file='../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/calPositions.RDS')
# write.csv(calPositions, file='../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/calPositions.csv')
# calPositions <- readRDS('../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/calPositions.RDS')
# calPositions <- read.csv('../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/calPositions.csv')
calStations <- loadStations('../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/', extraCols='TrackedGroup',
                            buoyPositions = '../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/calPositions.csv')
# Sette
setteStations <- loadStations('../SonoBuoy/Data/HICEAS_2017/Sette/Database/')
# Lasker
laskerStations <- loadStations('../SonoBuoy/Data/HICEAS_2017/Lasker/Database/')
# Test
testStations <- loadStations('./devel/final db formatting/', dateFormat = '%m/%d/%Y %H:%M', buoyPositions = file.choose())

# All the P1 P2 shit is messed up. Gotta combine all janky like
calSum <- detectionSummary(calStations) %>%
  mutate(Station = gsub('_P[1-9]$', '', Station)) %>%
  group_by(Buoy, Station, Species) %>%
  summarise(Latitude = median(Latitude), Longitude = median(Longitude),
            UTC = median(UTC), Count = sum(Count), UniqueCount = sum(UniqueCount)) %>%
  data.frame()

map <- getMap(calSum)
mapDetections(calSum, map=map, palette = 'YlOrRd', combine=T, nGroups = 9, species=c('bma', 'bmb'))

mapStations(setteStations)
mapDetections(detectionSummary(setteStations[-2:-1]))

# Drift
dtest <- driftCalibration(station$buoys)
# DRiFT on ReAL DAtA
sixTwenty <- loadStations('../SonoBuoy/Data/PAST_20170620')
ggplot(sixTwenty$`PAST20Jun2017_pg11511_sbExperiment DIFAR.sqlite3`$buoys$`0`$calibration[1:30,],
       aes(x=UTC, y=offset)) + geom_point()

filtBuoys <- lapply(sixTwenty$`PAST20Jun2017_pg11511_sbExperiment DIFAR.sqlite3`$buoys, function(b) {
  filtered <- b$calibration[1:20,]
  b$calibration <- filtered
  b
})

dtest <- driftCalibration(sixTwenty)
dtest <- driftCalibration(filtBuoys)
end <- endPoint(sixTwenty$`PAST20Jun2017_pg11511_sbExperiment DIFAR.sqlite3`$buoys,
                dtest, 20, 0)

lapply(dtest, function(x) sqrt(diag(solve(-x$hessian))))

boat <- calStations$`1647_SB_S4S5s.sqlite3`$buoys$`0`$calibration
start <- calStations$`1647_SB_S4S5s.sqlite3`$buoys$`0`$position[1,]
rates <- seq(0,3, length.out=30)
angles <- seq(0,360, length.out=120)
grid <- sapply(rates, function(r) {sapply(angles, function(t) {driftLogl(boat, start, c(r, t))})})
driftData <- data.frame(rate=unlist(lapply(rates, function(r) rep(r, length(angles)))), angle=rep(angles, length(rates)))
for(i in 1:nrow(driftData)) {
  driftData$logl[i] <- driftLogl(boat, start, c(driftData$rate[i], driftData$angle[i]))
}
plot_ly(x=driftData$rate, y=driftData$angle, color=driftData$logl) %>% add_surface()
plot_ly(z=grid) %>% add_surface()

# Bearing drawing
source('./devel/drawBearing.R')
library(manipulate)
myStation <- laskerStations$`1705_pg11511_sb_31_20171026.sqlite3`
myStation <- calStations$`1647_SB_S4S5s.sqlite3`
dets <- myStation$detections
buoys <- myStation$buoys
dets <- cbind(dets, buoyPosition(dets, transpose(buoys)$position))
drawBearings(dets, map=F)

station <- checkCalibrations(station)
checkCalibrations(station)

endPoint <- function(buoys, drift, endNum, buoyNum) {
  buoys <- buoys[[as.character(buoyNum)]]
  drift <- drift[[as.character(buoyNum)]]
  startTime <- buoys$position$UTC[1]
  endTime <- buoys$calibration$UTC[endNum]
  distance <- difftime(startTime, endTime, units='secs')*drift$rate/3600
  end <- swfscMisc::destination(buoys$position$Latitude[1],
                                buoys$position$Longitude[1],
                                drift$bearing, distance, units='km')
  data.frame(Latitude=c(end[1], buoys$position$Latitude[1]),
             Longitude=c(end[2], buoys$position$Longitude[1]),
             Time=c(startTime, endTime), Point=c('Start', 'End'))
}


