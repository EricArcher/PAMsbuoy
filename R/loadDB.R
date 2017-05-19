#' @title Load DIFAR database
#' @description Load list of data frames in DIFAR database
#'
#' @param fname filename of SQLite database with DIFAR data
#' @param verbose show database structure?
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom RSQLite dbConnect SQLite dbReadTable dbListTables dbDisconnect
#' @export
#'
loadDB <- function(fname = NULL, verbose = FALSE) {
  if(is.null(fname)) fname <- file.choose()
  con <- dbConnect(SQLite(), fname)
  db <- sapply(
    dbListTables(con),
    function(tbl) {
      df <- dbReadTable(con, tbl)
      if("UTC" %in% colnames(df) & is.character(df$UTC)) {
        df$UTC <- as.POSIXct(df$UTC, tz = "GMT")
      }
      df
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  dbDisconnect(con)
  attr(db, "station") <- basename(fname)

  db$Listening_Effort$Status <- tolower(str_trim(db$Listening_Effort$Status))
  db$DIFAR_Localisation$Species <- tolower(str_trim(db$DIFAR_Localisation$Species))

  if(verbose) print(str(db))
  invisible(db)
}