rm(list = ls())
library(PAMsbuoy)

folder <- "final db formatting"
survey <- loadStations(folder)

save.image("function test.rdata")

db <- loadDB("final db formatting/FinalFormat_Station1.sqlite3")
i <- which(sapply(db, function(x) any(colnames(x) %in% "CopyId")))
