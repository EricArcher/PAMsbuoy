rm(list = ls())
library(PAMsbuoy)

folder <- "calcurceas/effort"

effortAll <- loadEffort(folder)
##Only keep part one of "S43S44s" bcs calibration gets lost##
effortAll <- effortAll[effortAll$file != "S43S44s_P2", ]

effort <- effortDuration(effortAll)

save(effortAll, effort, file = "effort.rdata")
