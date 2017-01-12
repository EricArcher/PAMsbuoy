#' @title Load DIFAR data
#' @description Load DIFAR data from SQLite database.
#'
#' @param db.fname filename of SQLite database with DIFAR data
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom RSQLite dbConnect SQLite dbReadTable dbDisconnect
#' @export
#'
loadDifar <- function(db.fname) {
  con <- dbConnect(SQLite(), db.fname)
  difar <- dbReadTable(con, "DIFAR_Localisation")
  dbDisconnect(con)

  # remove extraneous spaces in data
  difar$TriggerName <- gsub(" ", "", difar$TriggerName)
  difar$Species <- gsub(" ", "", difar$Species)
  difar$MatchedAngles <- gsub(" ", "", difar$MatchedAngles)
  difar$TrackedGroup <- gsub(" ", "", difar$TrackedGroup)

# Q: Is calibration data available to be included here?

# Q: Is the MatchedAngles column the same structure as in the future?
  difar$detection <- labelDetection(difar)
  difar
}
