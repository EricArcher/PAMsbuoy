rm(list = ls())
library(PAMsbuoy)

# Q: will noise data by SQLite?

folder <- "calcurceas/noise"

noise <- loadNoise(folder)

save(noise, file = "noise.rdata")
