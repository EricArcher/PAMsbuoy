rm(list = ls())
library(RSQLite)
source("sb funcs.R")

fname <- "PAST_20160607_POST_PB_Edited.sqlite3"
difar <- loadDifar(fname)

difar <- clipBuoyLatLon(difar, lat.range = c(30, max(difar$BuoyLatitude)))

buoy.location <- buoyLoc(difar)
