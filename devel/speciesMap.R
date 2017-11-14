library(dplyr)
library(ggplot2)
library(ggmap)

calSum <- speciesSummary(calCur)
# calSum <- speciesSummary(lasker) %>% filter(Longitude < -140)
calMap <- getMap(calSum)

calToUse <- calSum %>% group_by(Station, Species) %>%
  summarise(Latitude=median(Latitude), Longitude=median(Longitude),
            Count = sum(Count)) %>% data.frame() %>%
  filter(Species == 'bp')

# calToUse <- calSum %>% group_by(Station) %>%
#   summarise(Latitude = median(Latitude), Longitude=median(Longitude),
#             Count=sum(Count)) %>% data.frame()

# Add check if we have too many levels
# Should we just break evenly into fixed number? Or incorporate histogram still?
sd <- round(sd(calToUse$Count))
breaks <- c(0,seq(1, max(calToUse$Count)+sd, sd))
breaks[length(breaks)] <- min(breaks[length(breaks)], max(calToUse$Count))
breaks <- unique(breaks)
calToUse$Breaks <- cut(calToUse$Count, breaks, ordered_result = TRUE, right=FALSE, include.lowest = TRUE)
haveLevels <- sort(as.numeric(unique(calToUse$Breaks)))
haveLabels <- c('[0]', levels(calToUse$Breaks)[-1])[haveLevels]
myPalette <- c('red', brewer.pal(length(breaks)-2, 'Reds'))
usePalette <- myPalette[haveLevels]

calMap + geom_point(data=filter(calToUse, Count > 0), aes(x=Longitude, y=Latitude, color=Breaks), size=3) +
  geom_point(data=filter(calToUse, Count==0), aes(x=Longitude, y=Latitude, color=Breaks),shape=4, size=3) +
  facet_wrap(~Species, nrow=2) +
  scale_color_manual(values=usePalette, labels=haveLabels) +
  labs(x='Longitude', y='Latitude', color='Count') +
  guides(color=guide_legend(override.aes = list(shape=c(4, rep(16, length(unique(calToUse$Breaks))-1))))) +
  theme(legend.key = element_rect(fill='#A3CCFF'))



