rm(list = ls())
library(PAMsbuoy)

folder <- "calcurceas/noise"

noise <- loadNoise(folder)

save(noise, file = "noise.rdata")
