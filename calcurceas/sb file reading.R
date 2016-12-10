# read S1025103s.csv into a data.frame (called 'difar')
# and create buoy0 and buoy1 objects below
# read.csv
# subset
# index a data.frame: [, ]
# colnames

# clear workspace
rm(list = ls())

#Station Location
# do this ---
# subset does....
# after subset, buoy0 is a .... (type of object) that has ....
buoy0 <- subset(difar, difar$Channel == 0)
colnames(buoy0)[colnames(buoy0)=="BuoyLatitude"] <- "BuoyLat0"
colnames(buoy0)[colnames(buoy0)=="BuoyLongitude"] <- "BuoyLong0"

buoy1 <- subset(difar, difar$Channel == 1)
colnames(buoy1)[colnames(buoy1)=="BuoyLatitude"] <- "BuoyLat1"
colnames(buoy1)[colnames(buoy1)=="BuoyLongitude"] <- "BuoyLong1"
# ----
