


calSum <- speciesSummary(calCur)

calMap <- getMap(calSum)

calToUse <- filter(calSum, Species=='bp')

calToUse <- calSum %>% group_by(Station, Species) %>%
  summarise(Latitude=median(Latitude), Longitude=median(Longitude),
            Count = sum(Count)) %>% data.frame() %>%
  filter(Species == 'bmb')

sd <- sd(calToUse$Count)
breaks <- seq(0, max(calToUse$Count)+sd, sd)
calToUse$Breaks <- cut(calToUse$Count, breaks, ordered_result = TRUE, include.lowest = TRUE)

calMap + geom_point(data=filter(calToUse, Count > 0), aes(x=Longitude, y=Latitude, color=Breaks), size=3) +
  geom_point(data=filter(calToUse, Count==0), aes(x=Longitude, y=Latitude), shape=4, color='red', size=3) +
  facet_wrap(~Species, nrow=2) +
  # scale_color_gradientn(colors=viridis(256, option='inferno', direction=-1))
  scale_color_brewer(palette='YlOrRd')


