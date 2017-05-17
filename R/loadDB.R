#' @title Load DIFAR database
#' @description Load list of data frames in DIFAR database
#'
#' @param db.fname filename of SQLite database with DIFAR data
#' @param db.str show database structure?
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom RSQLite dbConnect SQLite dbReadTable dbListTables dbDisconnect
#' @export
#'
loadDB <- function(db.fname = NULL, db.str = FALSE) {
  if(is.null(db.fname)) db.fname <- file.choose()
  con <- dbConnect(SQLite(), db.fname)
  db <- sapply(
    dbListTables(con),
    function(tbl) dbReadTable(con, tbl),
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  dbDisconnect(con)

  db$HydrophoneStreamers$UTC <- as.POSIXct(db$HydrophoneStreamers$UTC, tz = "GMT")
  db$DIFAR_Localisation$UTC <- as.POSIXct(db$DIFAR_Localisation$UTC, tz = "GMT")
  db$gpsData$UTC <- as.POSIXct(db$gpsData$UTC, tz = "GMT")

  if(db.str) print(str(db))
  invisible(db)
}