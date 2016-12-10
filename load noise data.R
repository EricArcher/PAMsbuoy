
###########
## NOISE ##
###########
setwd(dir.noise)
x <- c("NULL", NA, "NULL", "NULL", "NULL",  "NULL", NA, "NULL", "NULL","NULL", "character")

read.data <- function(file){
  dat <- read.csv(file,header=T,sep=",", colClasses=x )
  na.strings=c(""," ","NA")
  dat$stationAll <- file_path_sans_ext(file)
  return(dat)
}

annotate <- ldply(list.files(dir.noise), failwith(,read.data))
annotate <- with(annotate, cbind(annotate[1:4], colsplit(annotate$stationAll, pattern = "_P",  names = c('station', 'station.Part'))))
noise <-annotate[grepl("noise", annotate$notes, ignore.case=TRUE),]
#NOTE: ok to have 6 Errors "data has 0"
#No other errors are acceptable

setwd(dirname)
save(noise, file = "noise.rdata")
