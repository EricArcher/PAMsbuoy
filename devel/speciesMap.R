library(dplyr)
library(ggplot2)
library(ggmap)
library(tidyr)
library(RColorBrewer)
calCur <- readRDS('./devel/calCur.RDS')
lasker <- readRDS('./devel/lasker.RDS')
calSum2 <- detectionSummary(calCur) %>%
  mutate(Station = gsub('_P[1-9]$', '', Station))
calSum <- speciesSummary(lasker) %>% filter(Longitude < -140)
calSum <- filter(calSum, Latitude < 40)
calMap <- getMap(calSum)

calToUse <- calSum %>% group_by_(.dots=c('Station', 'Species')) %>%
  summarise(Latitude=median(Latitude), Longitude=median(Longitude),
            Count = sum(Count)) %>% data.frame() %>%
  filter(Species == 'bp')

calToUse <- calSum %>% group_by(Station) %>%
  summarise(Latitude = median(Latitude), Longitude=median(Longitude),
            Count=sum(Count)) %>% data.frame() %>%
  mutate(Species='All')

# Add check if we have too many levels
# Should we just break evenly into fixed number? Or incorporate histogram still?
sd <- round(sd(calToUse$Count))
breaks <- c(0,seq(1, max(calToUse$Count)+sd, sd))
breaks[length(breaks)] <- min(breaks[length(breaks)], max(calToUse$Count))
breaks <- unique(breaks)
calToUse$Breaks <- cut(calToUse$Count, breaks, ordered_result = TRUE, right=FALSE, include.lowest = TRUE)
haveLevels <- sort(as.numeric(unique(calToUse$Breaks)))
haveLabels <- c('0', levels(calToUse$Breaks)[-1])[haveLevels]
haveLabels <- gsub('(\\[|\\]|\\(|\\))', '', haveLabels)
haveLabels <- gsub(',', ' to ', haveLabels)
myPalette <- c('red', brewer.pal(length(breaks)-2, 'Reds'))
usePalette <- myPalette[haveLevels]

calMap + geom_point(data=calToUse, aes(x=Longitude, y=Latitude, color=Breaks, shape=(Count==0)), size=3) +
  facet_wrap(~Species, nrow=2) +
  scale_color_manual(values=usePalette, labels=haveLabels) +
  scale_shape_manual(values=c(16, 4), guide=FALSE) +
  labs(x='Longitude', y='Latitude', color='Detections') +
  guides(color=guide_legend(override.aes = list(shape=c(4, rep(16, length(unique(calToUse$Breaks))-1))))) +
  theme(legend.key = element_rect(fill='#A3CCFF'))






