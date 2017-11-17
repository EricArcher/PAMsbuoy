#' @title Load Multiple Stations
#' @description Load a list of sonobuoy stations from SQLlite databases in a folder
#'
#' @param folder folder containing SQLite databases with DIFAR data. if missing
#'   a dialog box will be presented to choose a folder
#'
#' @return a list of sonobuoy stations
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom tcltk tk_choose.dir
#' @importFrom tools list_files_with_exts
#' @export
#'
loadStations <- function(folder, db.ext = "sqlite3", ...) {
  if(missing(folder)) folder <- tcltk::tk_choose.dir()
  if(is.na(folder)) stop("No folder chosen")

  log.fname <- "PAMsbuoy_loadStations_log.txt"
  log <- file(log.fname, open = "wt")
  sink(log, type = "message")
  fnames <- tools::list_files_with_exts(folder, exts = db.ext)
  fnames <- fnames[order(nchar(fnames), fnames)]
  st.list <- sapply(seq_along(fnames), function(f) {
    if((f %% 10)==1) {
      cat('Loading station ', f, ' of ', length(fnames),'. \n')
    }
    message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ":", fnames[f])
    formatStation(loadDB(fnames[f], FALSE), ...)
  }, simplify = FALSE)
  sink(type = "message")
  file.show(log.fname)

  names(st.list) <- basename(fnames)
  attr(st.list, "survey") <- folder
  st.list
}