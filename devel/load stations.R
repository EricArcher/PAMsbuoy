rm(list = ls())
library(PAMsbuoy)

fname <- "RTsonobuoy_1_15_09.sqlite3"

db <- loadDB(fname)
station <- formatStation(db)
save.image("station test.rdata")