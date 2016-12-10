rm(list = ls())
source("sb funcs.R")

folder <- "calcurceas/noise"

noise <- loadNoise(folder)

save(noise, file = "noise.rdata")
