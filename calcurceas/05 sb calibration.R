
## Retrieve, compile and format data for analysis##

dirname <- "F:/r_19Feb"
setwd(dirname)
source("00sb funcs.R")


# Stations w Bad Calibrations
setwd(dirname)
badCalibrationAll <- read.csv("1647_SB_Calibration_Fixes.csv")
badCalibration <- subset(badCalibrationAll, badCalibrationAll$minutesAfterStartEffort ==0)

setwd(dir.difar)

difar <- ldply(list.files(dir.difar), failwith(,read.data))
difar <- with(difar, cbind(difar[1:22], colsplit(difar$stationAll, pattern = "_P", names = c('station', 'station.Part'))))
difar <- subset(difar, difar$BuoyLatitude>30) #to remove spurious -35 Buoy Latitude

difar0 <- subset(difar, difar$Channel == 0)

cal <- difar0[grepl("Vessel", difar0$Species, ignore.case=TRUE),]
which(colnames(cal) =="stationAll")
which(colnames(cal) =="Species")
which(colnames(cal) =="DIFARBearing")


calibration <- cal[,c(12, 17,22)]

calibrationSD_rough<- aggregate(calibration$DIFARBearing, by=list(name=calibration$stationAll), sd, simplify=TRUE)
calibration_under20 <- subset(calibrationSD_rough, calibrationSD_rough$StDev<20)
hist(calibration_under20$StDev, breaks = 20)

write.csv(calibrationSD_rough, "calibrationSD_rough.csv")

          