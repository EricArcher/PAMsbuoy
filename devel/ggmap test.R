library(ggmap)

pt_map <- get_map(
  location = c(-166.93, 23.97),
  scale = 2,
  zoom = 4,
  maptype = "satellite"
)

ran.pts <- data.frame(
  latitude = runif(100, 18, 28),
  longitude = runif(100, -180, -150)
)

ggmap(pt_map) +
  geom_point(data = ran.pts, aes(x = longitude, y = latitude), color = "yellow") +
  lims(x = range(ran.pts$longitude), y = range(ran.pts$latitude))