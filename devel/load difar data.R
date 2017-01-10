rm(list = ls())
library(PAMsbuoy)

# Q: should file.choose be used to select sqlite file?

fname <- "playback sea trial/PAST_20160607_POST_PB_Edited.sqlite3"

difar <- loadDifar(fname)
difar <- clipBuoyLatLon(difar, lat.range = c(30, max(difar$BuoyLatitude)))
buoy.location <- buoyLoc(difar)

save(difar, buoy.location, file = "difar.rdata")


# Q: what is structure of data with TrueBearing angles?

#Check that all TrueBearing angles are <360 deg
for (i in 1:nrow(difar)){
  b <- as.numeric(difar$TrueBearing[i])
  ifelse (b>=360, difar$TrueBearing[i]<- b-360, difar$TrueBearing[i]<-difar$TrueBearing[i])
  ifelse (b<0, difar$TrueBearing[i] <- b + 360, difar$TrueBearing[i] <- difar$TrueBearing[i])
}


# Q: "badAngle" assumes geometry of 2 buoys per detection - do we want to make this more generic?

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
