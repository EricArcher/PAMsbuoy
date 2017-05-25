#' @title Load Multiple Stations
#' @description Load a list of sonobuoy stations from SQLlite databases in a folder
#'
#' @param folder folder containing SQLite databases with DIFAR data. if missing
#'   a dialog box will be presented to choose a folder
#' @param verbose show station loading progress and summary after loading?
#'
#' @return a list of sonobuoy stations
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom tcltk tk_choose.dir
#' @importFrom tools list_files_with_exts
#' @export
#'
loadStations <- function(folder, verbose = TRUE, db.ext = "sqlite3") {
  if(missing(folder)) folder <- tcltk::tk_choose.dir()
  if(is.na(folder)) stop("No folder chosen")

  fnames <- tools::list_files_with_exts(folder, exts = db.ext)
  fnames <- fnames[order(nchar(fnames), fnames)]
  st.list <- sapply(fnames, function(f) {
    if(verbose) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ":", f, "\n")
    db <- loadDB(f, FALSE)
    formatStation(db)
  }, simplify = FALSE)
  names(st.list) <- basename(names(st.list))
  attr(st.list, "survey") <- folder
  st.list
}