rm(list = ls())
library(PAMsbuoy)

fname <- "RTsonobuoy_1_15_09.sqlite3"
db <- loadDB(fname)
station <- formatStation(db)

survey <- loadStations("sample survey")

save.image("function test.rdata")