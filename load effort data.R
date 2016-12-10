

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
