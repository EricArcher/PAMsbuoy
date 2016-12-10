#' @title Read station file
#' @description Read a file with station data into data.frame.
#'
#' @param f .csv file of station data
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @export
#'
readStationFile <- function(f) {
  df <- read.csv(f, na.strings = c("", " ", "NA"), stringsAsFactors = FALSE)
  if(nrow(df) == 0) return(NULL)
  dt <- paste0(df$UTC, ".", df$UTCMilliseconds)
  df$file <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(f))
  df$station <- gsub("_P[[:alnum:]]*", "", df$file)
  df$datetime <- strptime(dt, format = "%m/%d/%Y %H:%M:%OS", tz = "GMT")
  df
}
