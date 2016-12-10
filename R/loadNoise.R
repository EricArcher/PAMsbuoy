#' @title Load noise data
#' @description Load noise data from folder.
#'
#' @param folder folder containing .csv files of noise
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
loadNoise <- function(folder) {
  fnames <- dir(folder, full.names = TRUE)
  df <- do.call(rbind, lapply(fnames, readStationFile))
  df$notes <- sub("[[:space:]]+$", "\\1", df$notes)
  has.noise <- grep("noise", df$notes, ignore.case = TRUE)
  if(length(has.noise) == 0) return(NULL)
  df[has.noise, ]
}
