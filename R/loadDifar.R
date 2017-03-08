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

# Q: Multiple time columns - not sure whether we want to use UTC or PCLocalTime (PCTime is *not* good)
# A: In our experiment, I used PCLocalTime because that's what our Buoy's GPS was in. This is something
# that we probably want to think about and standardize.
  difar$posixDate <- as.POSIXct(difar$PCLocalTime)

# Q: Is the MatchedAngles column the same structure as in the future?
# A: We may need to re-create this column ourselves. Currently PAMGuard can only produce these if our
# data is in a specific order, otherwise it will always spit out NA's. There is also a known bug in their process
# (per comments in the Java source) which is creating all the NAs we see in our data. I have a rough outline of how
# it is currently calculating this column - it creates a score based on frequency and time overlap of
# two signals to decide which call(s) it most likely is the same as, looping through all previous calls.
# Not sure if we can replicate this process exactly with the data we have - we currently don't have a way
# (that I know of) to get the total frequency range of the call, only the specific chosen frequency. It
# could be somewhere in a different table.

  difar$detection <- labelDetection(difar)
  difar
}
