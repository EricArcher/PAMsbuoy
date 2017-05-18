#' @title Load DIFAR database
#' @description Load list of data frames in DIFAR database
#'
#' @param fname filename of SQLite database with DIFAR data
#' @param show.str show database structure?
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom RSQLite dbConnect SQLite dbReadTable dbListTables dbDisconnect
#' @export
#'
loadDB <- function(fname = NULL, show.str = FALSE) {
  if(is.null(fname)) fname <- file.choose()
  con <- dbConnect(SQLite(), fname)
  db <- sapply(
    dbListTables(con),
    function(tbl) dbReadTable(con, tbl),
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  dbDisconnect(con)

  db$DIFAR_Localisation$Species = tolower(str_trim(db$DIFAR_Localisation$Species))
  db$DIFAR_Localisation$UTC <- as.POSIXct(db$DIFAR_Localisation$UTC, tz = "GMT")
  db$HydrophoneStreamers$UTC <- as.POSIXct(db$HydrophoneStreamers$UTC, tz = "GMT")
  db$gpsData$UTC <- as.POSIXct(db$gpsData$UTC, tz = "GMT")

  if(show.str) print(str(db))
  invisible(db)
}