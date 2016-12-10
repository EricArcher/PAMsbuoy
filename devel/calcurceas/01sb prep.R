## Sonobuoy Density Estimation##
## Data Prep ##

### Create stations, difar, effort, and noise objects ###
rm(list = ls())

dirname <- "F:/r_19Feb"
setwd(dirname)
source("00sb funcs.R")

##############
## STATIONS ##
##############
setwd(dir.difar)

difar <- ldply(list.files(dir.difar), failwith(,read.data))
difar <- with(difar, cbind(difar[1:22], colsplit(difar$stationAll, pattern = "_P", names = c('station', 'station.Part'))))
difar <- subset(difar, difar$BuoyLatitude>30) #to remove spurious -35 Buoy Latitude

#Station Location
# do this ---
buoy0 <- subset(difar, difar$Channel == 0)
colnames(buoy0)[colnames(buoy0)=="BuoyLatitude"] <- "BuoyLat0"
colnames(buoy0)[colnames(buoy0)=="BuoyLongitude"] <- "BuoyLong0"

buoy1 <- subset(difar, difar$Channel == 1)
colnames(buoy1)[colnames(buoy1)=="BuoyLatitude"] <- "BuoyLat1"
colnames(buoy1)[colnames(buoy1)=="BuoyLongitude"] <- "BuoyLong1"
# ----

difar<- rbind.fill(buoy0, buoy1)

sLoc <- unique(difar[,c('station', 'BuoyLat0', 'BuoyLong0', 'BuoyLat1','BuoyLong1')])
stationLoc <- aggregate(sLoc, by=list(name=sLoc$station), min, na.rm=TRUE)

st1<- matrix(c(stationLoc$BuoyLong0, stationLoc$BuoyLat0), ncol=2)
st2<- matrix(c(stationLoc$BuoyLong1, stationLoc$BuoyLat1), ncol=2)
stest <- as.data.frame(midPoint(st1, st2))
stationLoc <-cbind(stationLoc, stest)
stationLoc <- rename(stationLoc, c("lat" = "stationLat", "lon" = "stationLong"))

##Determine Bad Angle for each buoy (badAngle) & buoy Separation (buoySepKM)
for (i in 1:nrow(stationLoc)){
  Lat0 <- stationLoc$BuoyLat0[i]
  Long0 <- stationLoc$BuoyLong0[i]
  Lat1 <- stationLoc$BuoyLat1[i]
  Long1 <- stationLoc$BuoyLong1[i]
  stationLoc$badAngle0[i] <- as.numeric(swfscMisc::bearing(Lat1, Long1, Lat0, Long0)[1])
  stationLoc$badAngle1[i] <- as.numeric(swfscMisc::bearing(Lat0, Long0, Lat1, Long1)[1])
  stationLoc$buoySepKM[i]<- swfscMisc::distance(Lat0, Long0, Lat1, Long1, units="km")
}

# Calibration Errors- Exclude stations w/ bad calibrations at start effort
##Note: files w/ delayed calibration errors will have altered end effort##

setwd(dirname)
badCalibrationAll <- read.csv("1647_SB_Calibration_Fixes.csv")
badCalibration <- subset(badCalibrationAll, badCalibrationAll$minutesAfterStartEffort ==0)

if (any(stationLoc$station %in% badCalibration$Station)) warning("CALIBRATION ERROR- REQUIRE MODIFICATION!!")

stations <- subset(stationLoc, !stationLoc$station %in% badCalibration$Station)


setwd(dirname)
save(stations, file = "stations.rdata")


############
## Effort ##
############

###To Do###
###CHANGE TO ELIMINATE EVERYTHING AFTER 'MINUTESAFTERSTART COLUMN'###
setwd(dir.effort)

effortAll <- do.call(rbind, lapply(list.files(dir.effort),read.data))
effortAll <- with(effortAll, cbind(effortAll[1:8], colsplit(effortAll$stationAll, pattern = "_P",  names = c('station', 'station.Part'))))

effortAll <-subset(effortAll, !UpdateOf == 0)
effortAll <- effortAll[ , c(2,6,8,9,10)]

#Pair up Matching on/off effort into one row
on <- effortAll[c(TRUE, FALSE),]
on <- on [, c(1,3,4,5)]
names(on) <- c("UTCon", "stationALL", "station", "station.Part")
off <- effortAll [c(FALSE, TRUE),]
off <- off [, c(1,3,4,5)]
names(off) <- c("UTCoff", "stationALL", "station", "station.Part")
effortAll <- cbind(on, off)
#MUST CHECK that this is true, otherwise issues w/ previous step!
all(effortAll$stationALL == effortAll$stationALL.1)
effortAll <- effortAll[1:5]

##Only keep part one of "S43S44s" bcs calibration gets lost##
outStation <- "S43S44s_P2"
effortAll <- effortAll[!effortAll$stationALL==outStation,]


#Duration of each On/Off section
for (i in 1:nrow(effortAll)) {
  on <- mdy_hms (as.character(effortAll$UTCon[i]))
  off <- mdy_hms (as.character(effortAll$UTCoff[i]))
  duration <- interval(off, on)
  effortAll$interval <- duration
  effortAll$durationP[i] <- as.duration(duration) #note: for the stationPart
  effortAll$durationP[i] <- as.numeric(abs(duration))
  effortAll$durationMinP[i] <- effortAll$durationP[i]/60
}
#NOTE: ok to have error w/ "coercing interval to duration"


# Combine effort for station Parts
effort <-ddply(effortAll, "station", transform, durationMin=sum(durationMinP))
effort <- subset(effort, !duplicated(station))

# Exclude effort for stations w/ bad calibration
effort <- subset(effort, !effort$station %in% badCalibration$Station)


#Save file
setwd(dirname)
save(effort, file = "effort.rdata")


###########
## Difar ##
###########
setwd(dir.difar)

difar <- ldply(list.files(dir.difar), failwith(,read.data))
difar <- with(difar, cbind(difar[1:22], colsplit(difar$stationAll, pattern = "_P", names = c('station', 'station.Part'))))

# Exclude effort for stations w/ bad calibration
difar <- subset(difar, !difar$station %in% badCalibration$Station)

#Check that all TrueBearing angles are <360 deg
for (i in 1:nrow(difar)){
  b <- as.numeric(difar$TrueBearing[i])
  ifelse (b>=360, difar$TrueBearing[i]<- b-360, difar$TrueBearing[i]<-difar$TrueBearing[i])
  ifelse (b<0, difar$TrueBearing[i] <- b + 360, difar$TrueBearing[i] <- difar$TrueBearing[i])
}

#Find angle difference between TrueBearing and Bad Angle
 difar <- subset(difar, difar$station %in% stations$station)
 difar <- merge(difar, stations[,c("station", "badAngle0", "badAngle1")], "station")

for (i in 1:nrow(difar)){
  b0 <-convert.angle(difar$badAngle0[i], from = "degrees", to ="radians")  #bad angle station 0 as radians
  b1 <-convert.angle(difar$badAngle1[i], from = "degrees", to ="radians")  #bad angle Channel 1 as radians
  a <- convert.angle(difar$TrueBearing[i], from = "degrees", to="radians") #convert true bearing to Radians
  ifelse (difar$Channel[i] == 0, delta0 <- abs(atan2(sin(b0-a), cos(b0-a))), delta0 <- NA)
  ifelse (difar$Channel[i] == 1, delta1 <- abs(atan2(sin(b1-a), cos(b1-a))), delta1 <- NA)
  delta0 <- as.numeric(convert.angle(delta0, from= "radians", to="degrees"))
  delta1 <- as.numeric(convert.angle(delta1, from= "radians", to="degrees"))
  difar$deltaBlindAngle0[i] <- delta0
  difar$deltaBlindAngle1[i] <- delta1
}

for (i in 1:nrow(difar)){
  if (difar$Channel[i]==1 && !is.na(difar$deltaBlindAngle1[i])) difar$deltaBlindAngle0[i] <- difar$deltaBlindAngle0[i-1]
  if (difar$Channel[i]==0 && !is.na(difar$deltaBlindAngle0[i])) difar$deltaBlindAngle1[i] <- difar$deltaBlindAngle1[i+1]
}

## May need to check why there are NAs in my deltaBearing Angles (some are due to NAs in TrueBearing, but not all!)

#Range (Detections --> Station Location)
detections <- difar[!is.na(difar$Latitude),]
stLoc<- stations[, c(2, 7, 8)]
detections <- merge(detections, stLoc, "station")

lat1 <- matrix(detections$stationLat, ncol=1)
lon1 <- matrix(detections$stationLong, ncol=1)
lat2 <- matrix(detections$Latitude, ncol=1)
lon2 <- matrix(detections$Longitude, ncol=1)

x <- as.data.frame(distance(lat1, lon1, lat2, lon2, units = "km", method = "haversine"))
y <- as.data.frame(swfscMisc::bearing(lat1, lon1, lat2, lon2))
colnames(x) <- "rangeKM"
colnames(y)<- "callAngle"
detections <-cbind(detections, x)
detections <- cbind(detections, y)

#Range (Sonobuoy --> Station Location)
lat1 <- matrix(detections$BuoyLatitude, ncol=1)
lon1 <- matrix(detections$BuoyLongitude, ncol=1)
lat2 <- matrix(detections$Latitude, ncol=1)
lon2 <- matrix(detections$Longitude, ncol=1)

x1 <- as.data.frame(distance(lat1, lon1, lat2, lon2, units = "km", method = "haversine"))
colnames(x1) <- "Buoy2CallrangeKM"
detections <-cbind(detections, x1)

#Check tracked Groups
if(any(detections$TrackedGroup == "999")) warning("Tracked Groups Include 999")

#Check that all of the detections are in stations present in station object (should be TRUE)
all(detections$station %in% stations$station)

setwd(dirname)
save(detections, file = "detections.rdata")

###########
## NOISE ##
###########
setwd(dir.noise)
x <- c("NULL", NA, "NULL", "NULL", "NULL",  "NULL", NA, "NULL", "NULL","NULL", "character")

read.data <- function(file){
  dat <- read.csv(file,header=T,sep=",", colClasses=x )
  na.strings=c(""," ","NA")
  dat$stationAll <- file_path_sans_ext(file)
  return(dat)
}

annotate <- ldply(list.files(dir.noise), failwith(,read.data))
annotate <- with(annotate, cbind(annotate[1:4], colsplit(annotate$stationAll, pattern = "_P",  names = c('station', 'station.Part'))))
noise <-annotate[grepl("noise", annotate$notes, ignore.case=TRUE),]
#NOTE: ok to have 6 Errors "data has 0"
#No other errors are acceptable

setwd(dirname)
save(noise, file = "noise.rdata")

###TODO####

#NEED TO SUBSET BASED ON QUALITY## If I do, I will first need to check if ther are duplicates of the
#TrackedGroups for each station... that will affect this

