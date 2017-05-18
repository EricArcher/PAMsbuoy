library(PAMsbuoy)

#fname <- "PAST_20160607_POST_PB_Edited.sqlite3"
#fname <- "PAST_20160607_POST_VesselTime_Test_5-10.sqlite3"
fname <- "RTsonobuoy_1_15_09.sqlite3"

db <- loadDB(fname)
station <- formatStation(db)
save.image("station test.rdata")