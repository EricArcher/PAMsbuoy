#' @title Format DIFAR data
#' @description Format DIFAR data from SQLite database.
#'
#' @param df data.frame of DIFAR data from database
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @export
#'
formatDifar <- function(df) {
  # remove extraneous spaces in data
  df$TriggerName <- gsub(" ", "", df$TriggerName)
  df$Species <- gsub(" ", "", df$Species)
  df$MatchedAngles <- gsub(" ", "", df$MatchedAngles)
  df$TrackedGroup <- gsub(" ", "", df$TrackedGroup)

  df$detection <- labelDetection(df)
  df
}
