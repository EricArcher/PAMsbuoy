#' @title Load DIFAR database
#' @description Load list of data frames in DIFAR database
#'
#' @param fname filename of SQLite database with DIFAR data. if \code{fname} is
#'   missing a dialog box will be presented to choose a filename
#' @param verbose show database structure after loading?
#'
#' @return a list of data frames representing all tables in the supplied
#'   SQLite database. Date/time values in columns labelled \code{UTC} will be
#'   converted to \code{POSIXct}
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom RSQLite dbConnect SQLite dbReadTable dbListTables dbDisconnect
#' @importFrom utils str
#' @export
#'
loadDB <- function(fname, verbose = FALSE) {
  if(missing(fname)) fname <- file.choose()
  if(!file.exists(fname)) stop(paste0("file '", fname, "' cannot be found."))
  con <- dbConnect(SQLite(), fname)
  db <- sapply(
    dbListTables(con),
    function(tbl) {
      df <- dbReadTable(con, tbl)
      if(is.character(df$UTC)) df$UTC <- as.POSIXct(df$UTC, tz = "GMT")
      df
    },
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  dbDisconnect(con)
  attr(db, "station") <- basename(fname)

  # check for necessary tables
  criticalTables <- c("DIFAR_Localisation", "HydrophoneStreamers", "gpsData")
  if(!all(criticalTables %in% names(db))) {
    stop(paste("Some necessary tables are missing. ", # in database ", fname,'. \n',
               'Missing tables are: ', paste(criticalTables[!(criticalTables %in% names(db))], collapse=' ')))
  }
  # These aren't critical to all the setup, just used for effort on/off and station info
  usefulTables <- c('Listening_Effort', 'Spectrogram_Annotation', 'Deploy', 'Sound_Acquisition')
  if(!all(usefulTables %in% names(db))) {
    missing <- usefulTables[!(usefulTables %in% names(db))]
    message(paste('Some useful tables are missing. ', # in database ', fname, '. \n',
                  'Missing tables are: ', paste(missing, collapse=' ')))
  }

  if(verbose) print(str(db))
  invisible(db)
}