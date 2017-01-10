#' @title Load effort data
#' @description Load effort data from folder.
#'
#' @param folder folder containing .csv files of effort
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @export
#'
loadEffort <- function(folder) {
  fnames <- dir(folder, full.names = TRUE)
  df <- do.call(rbind, lapply(fnames, readStationFile))

  df <- df[df$UpdateOf != 0, c("UTC", "UTCMilliseconds", "Comment", "station", "file", "datetime")]
  # remove trailing spaces from comments
  df$Comment <- sub("[[:space:]]+$", "\\1", df$Comment)
  # sort data.frame by station and date
  df <- df[order(df$station, df$datetime), ]
  # identify pairs of START/END effort
  paired.effort <- sapply(1:nrow(df), function(i) {
    if(df$Comment[i] == "START EFFORT") return(df$Comment[i + 1] == "END EFFORT")
    if(i > 1 & df$Comment[i] == "END EFFORT") return(df$Comment[i - 1] == "START EFFORT")
    FALSE
  })
  df[paired.effort, ]
}
