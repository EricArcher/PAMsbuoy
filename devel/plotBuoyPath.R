library(ggplot2)

con <- dbConnect(SQLite(), "RTsonobuoy_1_15_09.sqlite3")
hphone <- dbReadTable(con, "HydrophoneStreamers")
dbDisconnect(con)

df <- df[order(df$UTC), ]
ggplot(df, aes(BuoyLongitude, BuoyLatitude)) +
  geom_point(aes(color = factor(Channel)))