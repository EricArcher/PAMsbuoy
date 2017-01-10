rm(list = ls())
library(PAMsbuoy)

# Q: will effort be SQLite?

folder <- "calcurceas/effort"

effortAll <- loadEffort(folder)

# Q: is it necessary to delete parts of effort?

##Only keep part one of "S43S44s" bcs calibration gets lost##
effortAll <- effortAll[effortAll$file != "S43S44s_P2", ]

effort <- effortDuration(effortAll)

save(effortAll, effort, file = "effort.rdata")
