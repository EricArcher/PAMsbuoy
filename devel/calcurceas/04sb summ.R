## Sonobuoy Density Estimation##
## Summary ##

# Summaries for Sonobouy Density Estimation Project#

rm(list = ls())
dirname <- "F:/r_19Feb"
setwd(dirname)

source("00sb funcs.R")

load("LMRsonobuoyData.rdata")
load("pt.data.rdata")

################
## Histograms ##
################

hist(callsAll$rangeKM)

#########################
## Trial Summary Table ##
#########################
## finish this ##
read.csv (grepl("Trial_"))

############
## Effort ##
############

#Plot effort per station

ggplot(data = effort, aes(x = station, y = durationMin)) + 
  geom_bar(stat="identity", color = "blue", fill="steelblue") +
  coord_flip()+
  ylab ("Duration (minutes)") + xlab ("Station")+
  #  geom_text(aes(label=duration), vjust=1.6, color="white", size=10)+
  #theme (axis.text.y = element_text(angle = 45, size = 5, hjust = 0.5, vjust = 0, face = 'italic'))
  theme (axis.text.y = element_text(size = 5, hjust = 0.5, vjust = 0, face = 'italic'))

############
## Range  ##
############

# Purpose is to determine a truncation distance/range

unique(calls$Species)

#Choose the species you want to view
Spp <- "BmA"
title <- paste("Detection Function for", Spp)
Detection <-detections[grepl(Spp, detections$Species, ignore.case=TRUE),]
DetectionSpp <- subset(Detection, Detection$rangeKM<20)

# Distance Sampling for a Point #
names(DetectionSpp)[names(DetectionSpp)=="rangeKM"] <- "distance"
dsDetectionSpp <- ds(DetectionSpp, transect="point", dht.group = TRUE)

# Plot Detection Function
#plot(dsDetectionSpp,main= title)
plot(dsDetectionSpp)



##################
## Blind Angles ##
##################
#histogram range vs delta angle

blindAngleRange <- plot(callsAll$deltaBlindAngle1, callsAll$rangeKM)

##barChart polar (frequency at angles)
callAngleInterval <- cut_interval(callsAll$TrueBearing, length=10)

callAngleIntervalPolar <- ggplot(callsAll, aes(x=callAngleInterval, fill = callAngleInterval)) +
  geom_bar()+
  coord_polar(theta = "x")+
  theme_minimal()+
  guides(fill=FALSE)+
  scale_x_discrete(label=function(x){return(seq(0, 360, 10))})
callAngleIntervalPolar

##PolarPlot
#Center callAngle around the sonobuoy Axis-may wnat to move this to 01sb prep.R
#Yes, i think so-- then I can more easily subset calls vs callsAll
#callP <- pt.calls
callP <- callsAll20
for (i in 1:nrow(callP)){
  b <- callP$callAngle[i]
  st <- callP$station[i]
  bad0 <- stations[stations$station==st,][,"badAngle0"]
  bNorm <- as.numeric(b-bad0)
  if (bNorm < 0) bNorm=bNorm+360
  #calls$callAngleNorm[i] <- bNorm-90
  callP$callAngleNorm[i] <- bNorm
}

#Plot Polar Plot w/ centered call angles
Spp <- "Bp"
callP <-callP[grepl(Spp, callP$Species, ignore.case=TRUE),]
callD <- subset(callP, callP$rangeKM<r)
d <- callD$rangeKM #Distance from station location to call location
b <- callD$callAngleNorm #Angle from station location to call location


ggplot(data = callD) +
  geom_point(aes(x = b, y = d, color = Species), size = 2.2, alpha = 0.7) +
  geom_hline(yintercept = seq(0, r, by = 1), colour = "grey", size = .8) +
  geom_vline(xintercept = seq(0, 360-1, by = 45), colour = "grey", size = .8) +
  coord_polar(theta = 'x', start = 4.714, direction = 1) +
  labs(x = '', y = '') +
  scale_color_manual(values = c('turquoise2', 'chartreuse2' , 'magenta3'),
         labels = c('Blue Whale "B"', 'Fin Whale', 'Blue Whale "A"')) +

   # scale_color_manual(values = 'chartreuse2',
   #                    labels = 'Fin Whale') +
   # 
  scale_x_continuous(limits = c(0, 360), expand = c(0, 0), breaks = seq(0, 360-1, by = 45)) +
  scale_y_continuous(limits = c(0, r), breaks = seq(0, r, by = 1)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid  = element_blank())


##########
## MAPS ##
##########
#rm(list = ls())

library(maps)
library(mapdata)
library(ReporteRs)

maplon=c(-132, -116)
maplat=c(30, 49)

map("world",xlim=maplon,ylim=maplat,myborder=0,type="n",mar=c(7,8,0,0))


polygon(c(maplon[1],maplon[1],maplon[2],maplon[2]),c(maplat[1],maplat[2],maplat[2],maplat[1]),col="aliceblue")
# plot coast
coast <-"world" 
map("worldHires",xlim=maplon,ylim=maplat,fill=TRUE,col="gray90",resolution=0,bg=NA,add=T)
detach(2)
map("worldHires",boundary=FALSE,interior=TRUE,fill=FALSE,resolution=0,add=TRUE)

# add axes
axis(1,padj=-0.5)
axis(2,padj=0.5,las=1)
axis(3,labels=FALSE,tck=0)
axis(4,labels=FALSE,tck=0, line=)
title(xlab="Longitude",line=2)
title(ylab="Latitude",line=3)

stationsPlot <- subset(stations, stations$station %in% Stations2Use)
points(stationsPlot$stationLong, stationsPlot$stationLat, pch=19, col="red", cex=1)
map.scale(-130.3, 31.5, ratio=FALSE, relwidth=.2, cex=.8)

#dev.off()