# Testing zone.
library(PAMsbuoy)
library(dplyr)
db <- loadDB('../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/1647_SB_S102S103s.sqlite3')
db <- loadDB('../SonoBuoy/Data/PAST_20170620/PAST20Jun2017_pg11511_sbExperiment DIFAR - Circles.sqlite3')
db <- loadDB('../SonoBuoy/Data/PAST_20160607_POST_VesselCalOnly.sqlite3')
db <- loadDB('../SonoBuoy/Data/HICEAS_2017/Sette/Database/1706_pg11511_sb_10_20170722.sqlite3')
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
mapDetections(calSum, map=map, palette = 'YlOrRd', combine=T, nGroups = 9, species=c('bma', 'bmb'))

mapStations(setteStations)
mapDetections(detectionSummary(setteStations[-2:-1]))

# Drift
dtest <- driftCalibration(station$buoys)
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
