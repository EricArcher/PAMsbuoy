## Sonobuoy Density Estimation##
## Call Density Estimation (callDE) ##

# Analyze data to determine call density#
# Call Density = Calls per Area per Time
rm(list = ls())

dirname <- "F:/r_19Feb"
setwd(dirname)
source("00sb funcs.R")

load("effort.rdata")
load("noise.rdata")
load("detections.rdata")
load("stations.rdata")


################
## Arguements ##
################
r <-100  #truncation distance or radius of the point/circle
a <- 0
truncTime <- 35 # minutes from start time to include
spp <- "BmB"  #species
trial.name <- paste("Trial", paste (r, "km", sep=""), paste(a, "deg", sep=""), paste(truncTime, "min", sep=""), sep="_")


##########
## TIME ##
##########
#Good Stations
TIME <- subset(effort, effort$station %in% stations$station)
TIME <- subset(TIME, TIME$durationMin > truncTime)
Stations2Use <- unique(TIME$station)

#Find Noise in each station
Noise <- subset(noise, noise$station %in%TIME$station)
for (i in 1:nrow(Noise)){
  st <- Noise$station[i]
  eRow <- TIME[which(TIME$station == st),]
  on <- mdy_hms (as.character(eRow$UTCon))
  off <- on + minutes(truncTime)
  int <- interval (on, off)
  n <- mdy_hms(as.character(Noise$UTC[i]))
  Noise$inInterval[i] <- n %within% int
}  
any(Noise$inInterval==TRUE)

noiseSum <- as.data.frame(aggregate(Noise$Duration, by=list(Noise$station), FUN=sum))
noiseMin <-noiseSum[2]/60
noiseSum <- cbind(noiseSum, noiseMin)
colnames(noiseSum)[1] <- "station"
colnames(noiseSum)[2] <- "noiseSec"
colnames(noiseSum)[3] <- "noiseMin"

noise.all <- sum(noiseSum$noiseMin)

#Time in minutes
time.all <- if (truncTime < 120) {
  length(unique(Stations2Use))*truncTime
} else {
  sum(TIME$durationMin)
}

pt.time = time.all-noise.all
  

setwd(dirname)
save(pt.time, file = "pt.time.rdata")

##################
## Calls to Use ##
##################
# Eliminate detections with NAs
calls <- subset(detections, !is.na(detections$TrueBearing))
calls <- subset(calls, !is.na(detections$deltaBlindAngle0))
calls <- subset(calls, !is.na(detections$deltaBlindAngle1))

# Eliminate detections outside Truncation Range
calls <- subset(calls, calls$rangeKM < r)

# Eliminate calls that occur in stations with no effort
calls<- subset(calls, calls$station %in% effort$station)
all(calls$station %in% effort$station)
calls <- subset(calls, calls$station %in% Stations2Use)
all(calls$station %in% Stations2Use)

#Eliminate calls that occur outside timeTrunc
for (i in 1:nrow(calls)){
  call <- mdy_hms(calls$UTC[i])
  loc<- which(effort$station==calls$station[i], arr.ind=TRUE)
  start <- mdy_hms(as.character(effort$UTCon[loc]))
  end <- start + minutes(truncTime)
  duration <- interval (start, end)
  calls$inTruncTime[i]<- ifelse (call %within% duration, TRUE, FALSE) 
}

#Calls to use for graphics
callsAll <- calls # If I save this at this point, it should be good for graphics
callsAll.InTruncTime <- subset(callsAll, callsAll$inTruncTime==TRUE)

#Eliminate calls inside blind zone
calls <-subset(calls, calls$deltaBlindAngle0 > a & calls$deltaBlindAngle1 > a)


#Final Call Data
pt.calls <- subset(calls, calls$inTruncTime==TRUE)
pt.calls.BmB <- pt.calls[grepl("BmB", pt.calls$Species, ignore.case=TRUE),]
pt.calls.BmA <- pt.calls[grepl("BmA", pt.calls$Species, ignore.case=TRUE),]
pt.calls.Bp  <- pt.calls[grepl("Bp", pt.calls$Species, ignore.case=TRUE),]

setwd(dirname)
save(pt.calls, pt.calls.BmB, pt.calls.BmA, pt.calls.Bp, file = "pt.calls.rdata")


##########
## Area ##
##########
AreaTransect <-subset(stations, stations$station %in% Stations2Use)
all(pt.calls$station %in% AreaTransect$station) #make sure this is TRUE

for (i in 1:nrow(AreaTransect)){
  rWedge <- r - AreaTransect$buoySepKM[i]
  areaWedge <- 0.5*(a*(pi/180)*rWedge^2)
  areaCircle <- pi * r^2
  AreaTransect$areaWedge[i] <- areaWedge
  AreaTransect$areaStation[i] <- areaCircle + 2*areaWedge
}

pt.area <- sum(AreaTransect$areaStation)

setwd(dirname)
save(pt.area, file = "pt.area.rdata")

########
## DE ##
########
##To Do ## Not Working
callsDE <- calls
which(colnames(callsDE)== "rangeKM")
colnames(callsDE)[31] <- "distance"
halfnorm.calls <- ds(callsDE, key="hn", adjustment="cos", transect="point")
deplot <- plot(halfnorm.calls)

Spp <- "BmB"
detDE <-callsDE[grepl(Spp, callsDE$Species, ignore.case=TRUE),]
halfnorm.calls.BmB <- ds(detDE, key="hn", adjustment="cos", transect="point")
deplot.BmB <- plot(halfnorm.calls.BmB)

Spp <- "BmA"
detDE <-callsDE[grepl(Spp, callsDE$Species, ignore.case=TRUE),]
halfnorm.calls.BmA <- ds(detDE, key="hn", adjustment="cos", transect="point")
deplot.BmA <- plot(halfnorm.calls.BmA)

Spp <- "Bp"
detDE <-callsDE[grepl(Spp, callsDE$Species, ignore.case=TRUE),]
halfnorm.calls.Bp <- ds(detDE, key="hn", adjustment="cos", transect="point")
deplot.Bp <- plot(halfnorm.calls.Bp)

# tiff(filename = paste("DEplot_", trial.name, ".tif", sep=""), width=1098, height = 616, res=300)
# deplot <- plot(halfnorm.calls)
# dev.off()




##########
## SAVE ##
##########

save(stations, detections, noise, effort, calls, callsAll, AreaTransect, file="LMRsonobuoyData.rdata" )
save(Stations2Use, pt.time, pt.area, pt.calls, pt.calls.BmB, pt.calls.BmA, pt.calls.Bp, file = "pt.data.rdata")

#save trial summary info to later rbind together
headers.trial <- c("Trial", "Blind Angle", "Range (km)", "Truncation Time", "Number of Stations", 
                  "Total Time","Noise (min)", "Total Area", "All Calls (incl. Blind Angles)", 
                  "Total Calls", "Bm 'B' Calls","Bm 'A' Calls", "Bp calls")

trial <- c(trial.name, a, r, truncTime, length(Stations2Use), pt.time, pt.area, noise.all,
           nrow(callsAll.InTruncTime), nrow(pt.calls), nrow(pt.calls.BmB), nrow(pt.calls.BmA), nrow(pt.calls.Bp))
trialSumm <- rbind(headers.trial, trial)

setwd(dirname)
save(trialSumm, file = paste(trial.name, ".rdata", sep=""))
write.csv(trialSumm, file = paste(trial.name, ".csv", sep=""))
