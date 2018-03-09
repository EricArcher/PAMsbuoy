# Testing zone.
library(PAMsbuoy)
library(dplyr)
library(tools)
library(ggplot2)
source('../SonoBuoy/SonoBuoyFunctions.R')
library(lubridate)
library(viridisLite)
db <- loadDB('../SonoBuoy/Data/CalCurCEAS2014/CalCurCEAS_SonoBuoy/SQLite/1647_SB_S89S90s_P1.sqlite3')
db <- loadDB('../SonoBuoy/Data/PAST_20170620/PAST20Jun2017_pg11511_sbExperiment DIFAR - Circles.sqlite3')
db <- loadDB('../SonoBuoy/Data/PAST_20160607_POST_PB_Edited.sqlite3')
db <- loadDB('../SonoBuoy/Data/PAST_20160607_POST_VesselCalOnly.sqlite3')
db <- loadDB('../SonoBuoy/Data/HICEAS_2017/Sette/Database/1706_pg11511_sb_opp_20170924.sqlite3')
db <- loadDB('../SonoBuoy/Data/HICEAS_2017/Sette/Database/1706_pg11511_sb_40_20170924.sqlite3')
db <- loadDB('./devel/final db formatting/FinalFormat_Station2.sqlite3')

buoyPos <- data.frame(Buoy = '1', UTC='2014-08-08 03:19:27',
                      Latitude = 34.6, Longitude = -125)
station <- formatStation(db, overrideError = F, dateFormat = '%m/%d/%Y %H:%M')

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


# table rows 37px, top 59px
if(is.null(webshot:::find_phantom())) {
  webshot::install_phantomjs()
}


db <- loadDB('../SonoBuoy/Data/PAST_20160607_POST_VesselCalOnly.sqlite3')
difarTest <- formatStation(db, buoyPositions = spots)

db <- loadDB('../SonoBuoy/Data/PAST_20160607_POST_PB_Edited.sqlite3')
db$DIFAR_Localisation$Species[sample(1:nrow(db$DIFAR_Localisation), 200)] <- 'vessel'
difarTest <- formatStation(db, buoyPositions = spots)
# db$HydrophoneStreamers$DifarModuleAction <- 'deployed'

load('../SonoBuoy/difarData.Rds')
colnames(finalDifar)
finalDifar <- finalDifar %>%
  select(Buoy=Channel, BoatLatitude=BoatLat, BoatLongitude=BoatLong, UTC,
         BuoyLatitude, BuoyLongitude, DIFARBearing, Species, RealBearing, Distance) %>%
  mutate(UTC=ymd_hms(UTC), AngleError = (RealBearing-DIFARBearing) %% 360,
         AngleError = ifelse(AngleError > 180, AngleError-360, AngleError)) %>%
  rename(OldDifar = DIFARBearing, DIFARBearing=RealBearing)

firstStation <- formatStation(db, buoyPositions = spots)
testStation <- firstStation

# Breaks with too many points -
for(b in 1:4) {
  testStation$buoys[[b]]$calibration <- finalDifar %>% filter(Buoy==b-1) %>% head(50) %>% sample_n(10)
}

# Add some bias
testStation$buoys$`3`$calibration$DIFARBearing <- testStation$buoys$`3`$calibration$DIFARBearing +
  rnorm(nrow(testStation$buoys$`3`$calibration), 10, 10)

# Drifts - manually changed above and re-ran
driftsActualDifar <- driftCalibrationBias(testStation$buoys, inits=c(10,10,10))
driftsActualNoBias <- driftCalibration(testStation$buoys)
driftsReal <- driftCalibrationBias(testStation$buoys, inits=c(10,10,10))
driftsRealNoBias <- driftCalibration(testStation$buoys)

# Combine to DF
ds <- list(driftsActualDifar, driftsActualNoBias, driftsReal, driftsRealNoBias)
names(ds) <- c('Difar', 'DifarNoBias', 'Real', 'RealNoBias')
ddata <- do.call(rbind, lapply(names(ds), function(st) {
  tmp <- purrr::transpose(ds[[st]])
  bias <- if('bias' %in% names(tmp)) {
    unlist(tmp$bias)
  } else 0
  data.frame(Rate=unlist(tmp$rate), Angle=unlist(tmp$bearing), Buoy=0:3, Bias=bias, Type=st)
}))
ddata$Buoy <- as.character(ddata$Buoy)
# Add start
ddata$Latitude <- 0; ddata$Longitude <- 0; ddata$UTC <- ymd_hms('2017-04-04 04:04:04')
ddata$EndUTC <- ddata$UTC
for(i in 1:nrow(ddata)) {
  pos <- purrr::transpose(testStation$buoys)$position[[ddata$Buoy[i]]] %>% data.frame()
  cal <- purrr::transpose(testStation$buoys)$calibration[[ddata$Buoy[i]]] %>%
    data.frame() %>% arrange(desc(UTC))
  ddata$Latitude[i] <- pos[1, 'Latitude']
  ddata$Longitude[i] <- pos[1, 'Longitude']
  ddata$UTC[i] <- pos[1, 'UTC']
  ddata$EndUTC[i] <- cal[1, 'UTC']
}
endPoints <- mapply(swfscMisc::destination, ddata$Latitude, ddata$Longitude, ddata$Angle,
                                    ddata$Rate*difftime(ddata$EndUTC, ddata$UTC, units='secs')/3600,
                                    units='km')

ddata$EndLat <- endPoints[1,]; ddata$EndLong <- endPoints[2,]
# Plot daaaa drifts
ggplot() + geom_segment(data=ddata, aes(x=Longitude, y=Latitude, xend=EndLong, yend=EndLat, color=Type), size=3, alpha=.5) +
  geom_point(data=finalDifar, aes(x=BuoyLongitude, y=BuoyLatitude, shape=as.character(Buoy)), size=2)

#######################################
allDifar <- do.call(rbind, purrr::transpose(firstStation$buoys)$calibration) %>%
  group_by(Buoy) %>% top_n(-1, UTC)


spots %>% group_by(Buoy) %>% top_n(-2, UTC)
firstDifar <- db$DIFAR_Localisation %>% group_by(Channel) %>%
  top_n(-2, UTC) %>% select(UTC, Channel)

ggplot() + geom_point(data=spots, aes(x=Longitude, y=Latitude, color=Buoy, shape='Buoy')) +
  geom_point(data=allDifar, aes(x=BoatLongitude, y=BoatLatitude, color=Buoy, shape='Boat'))

plist <- vector('list', 4)
for(i in 1:4) {
  graphme <- testStation$buoys[[i]]
  plist[[i]] <- ggplot() +
    geom_point(data=graphme$position[1,], aes(x=Longitude, y=Latitude, color='Buoy')) +
    geom_point(data=graphme$calibration, aes(x=BoatLongitude, y=BoatLatitude, color='Boat'))
}
gridExtra::grid.arrange(plist[[1]], plist[[2]], plist[[3]], plist[[4]], nrow=2)

# How far
sixBuoys <- read.csv('../SonoBuoy/Data/PAST_20170620/Data/spot_messages.csv') %>%
  mutate(UTC=mdy_hm(datetime))
ggplotly(
  ggplot(sixBuoys, aes(x=Longitude, y=Latitude, color=UTC)) + geom_point()
)
# 8:42 to 15:57
sixBuoys <- filter(sixBuoys, UTC < ymd_hm('2017-06-20 15:57'), UTC > ymd_hm('2017-06-20 08:40'))
ggplotly(
  ggplot(sixBuoys, aes(x=Longitude, y=Latitude, color=UTC)) + geom_point()
)


#### the fuck happened
v1 <- -2; v2 <- -3
x <- exp(v1); y <- exp(v2)
x/(x+y); y/(x+y)

t <- 1:2
for(i in t) {
  if(2>t[i] |
     1<t[3]) {
    print('heyyy')
  }
}
test$Cruise[1:20] <- 'Yeahhhh'

# including 5 from vessel buts and 4 from vessel anus
myVes <- levels(as.factor(test$Cruise))
sapply(myVes, function(v) {
  num <- test %>% filter(Cruise==v) %>% distinct(Buoy, Station) %>% nrow()
  paste0(num, ' from vessel ', v)
}
) %>% formatListGrammar()
test %>% group_by(Cruise) %>% distinct(Buoy, Station) %>% summarise(n())

calStations$`1647_SB_S4S5s.sqlite3`$buoys$`0`$BuoyQuality

summary(factor(unlist(lapply(calStations, function(s) {
  lapply(s$buoys, function(b) {
    if(is.na(b$info$BuoyQuality)) {
      'None'
    } else b$info$BuoyQuality
  })
})), levels=c('Good', 'Bad', 'Questionable', 'None')))


kable(select(detSummary, -Station),  align='c', digits=2,
      col.names=myColumns[2:6], escape=FALSE, format='html') %>%
  kable_styling('bordered') %>%
  row_spec(odds, background='#edf0f4') %>%
  group_rows(index=id) %>%
  collapse_rows(1)

myTable <- PAMsbuoy:::makeHtmlTable(testSum)
tmp <- tempfile('tmpTable', fileext = '.html')
rmarkdown::render(system.file('templates/tableTemplate.Rmd', package='PAMsbuoy')
                  , tmp, output_format='html_document', quiet=TRUE)
webshot::webshot(tmp, file='wut.png', cliprect=c(0, 0, 1000, 3000))

# 58 header, 37 group head and row

# wincruz
# Event 'S' is first sight. Spps have species, summarise below. Sight should be number.
windas <- swfscMisc::das.read('./devel/wincruz/CalC1647.das')
swfscMisc::das.spp.freq(windas)
# spcode .dat file is a fwf
spp <- read.fwf('./devel/wincruz/SpCodes_2013.dat',
                widths=c(4, 11, 39), stringsAsFactors=FALSE)
colnames(spp) <- c('Code', 'ShortName', 'ScientificName')
spp <- mutate(spp, Code = str_trim(Code),
              ShortName = str_trim(ShortName),
              ScientificName = str_trim(ScientificName))

windas %>% filter(!is.na(Sight) & !is.na(Spp1)) %>% head() %>%
  mutate(test=paste0(Spp1, Spp2, Spp3, sep=','), test=gsub('NA', '', test)) %>%
  str()
win <- windas %>% filter(!is.na(Sight) & !is.na(Spp1)) %>%
  select(Code = Spp1, SightingId = Sight) %>% distinct()

setSum %>% mutate(SightingId = as.numeric(SightingId)) %>%
  left_join(win) %>% left_join(spp) %>% select(-UTC, -Station, -StationType, -Longitude) %>%
  data.frame() %>% head()

stationInfo <- data.frame(cruise=NA, instrument_type=NA,
                          instrument_id=NA, station_type=NA, vis_id=NA)

load('calStations.Rdata')
calStations <- lapply(calStations, function(s) {
  s$stationInfo <- stationInfo
  s$detections$CalibrationValue <- rep(NA, nrow(s$detections))
  s$detections$CalibratedBearing <- rep(NA, nrow(s$detections))
  s$buoys <- lapply(s$buoys, function(b) {
    b$info <- list(BuoyQuality = NA,
                   CalibrationType = NA,
                   Drift = NA)
    b
  })
  s
})
attr(calStations, 'survey') <- 'CalCurCEAS'

test <- 1:20
t <- c('stationList$Station1', 'stationList[1:10]', 'stationList[c(1,4,6)]')
splits <- str_split(t, '[\\$\\[]')
splits
eval(parse(text=gsub(']', '', splits[[3]][2])))
test[eval(parse(text=splits[[3]][2]))]

library(PamBinaries)
bintest <- loadPamguardBinaryFile(file.choose())

test <- data.frame(x=1:8, y=10:17, z=letters[1:8])
ggplot(test, aes(x=x, y=y)) + geom_point(aes(color=z), size=5) +
  scale_color_manual(values=c("#000000", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", '#000000'))
c("#000000", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# dark green is 4. black 1. 6 dark blue 7 dark orange
colorTest <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colorPalette <- c("#000000","#009E73", "#0072B2", "#D55E00", "#F0E442", "#CC79A7")
colorNames <- c('Black', 'Green', 'Blue', 'Orange', 'Yellow', 'Pink')
colorTest <- rep(colorTest,2)
topEight <- filter(setSum, Station %in% unique(setSum$Station)[1:4])
mapTest <- getMap(topEight)
mapTest + geom_point(data=topEight, aes(x=Longitude, y=Latitude, color=Station), size=5) +
  scale_color_manual(values=colorTest, labels=colorNames)
