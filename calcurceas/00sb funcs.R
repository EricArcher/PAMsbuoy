## Sonobuoy Density Estimation##
## Analysis Functions ##

# Analysis functions that will be applied to more than one script #

dirname <- "F:/r_19Feb"
setwd(dirname)

dir.effort = paste(dirname, "/", "effort",sep="")
dir.difar = paste(dirname, "/", "difar",sep="")
dir.noise = paste(dirname, "/", "noise",sep="")

## Install Libraries
#if (!require('devtools')) install.packages('devtools')
#devtools::install_github('ericarcher/swfscMisc')

 library(geosphere)
 library(reshape2)
 library(tools)
 library(swfscMisc)
 library(lubridate)
 library(plyr)
 library (ggplot2)
# library(Distance)
options(lubridate.verbose=FALSE)
options(digits=10, nsmall=6)

read.data <- function(file){
  dat <- read.csv(file,header=T,sep=",", na.strings="NA")
  na.strings=c(""," ","NA")
  dat$stationAll <- file_path_sans_ext(file)
  return(dat)
}


