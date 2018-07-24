#' @title Load Multiple Stations
#' @description Load a list of sonobuoy stations from SQLlite databases in a folder
#'
#' @param folder folder containing SQLite databases with DIFAR data. if missing
#'   a dialog box will be presented to choose a folder
#' @param db.ext database extension to look for in chosen folder
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
  error <- FALSE
  # need to wrap this in a try, otherwise if theres an error we dont close the sink connection
  cat('Loading stations... \n')
  pb <- txtProgressBar(min=0, max=length(fnames), style=3)
  try({
    st.list <- sapply(seq_along(fnames), function(f) {
    # if((f %% 10)==1 | f == length(fnames)) {
    #   cat('Loading station ', f, ' of ', length(fnames),'. \n')
    # }
    setTxtProgressBar(pb, f)
    message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ":", fnames[f])
    station <- formatStation(loadDB(fnames[f], FALSE), overrideError = TRUE, ...)
    for(b in seq_along(station$buoys)) {
      if(station$buoys[[b]]$error) {
        error <<- TRUE
      }
      station$buoys[[b]] <- station$buoys[[b]][!(names(station$buoys[[b]]) == 'error')]
    }
    station
  }, simplify = FALSE)
  })
  sink(type = "message")
  file.show(log.fname)
  if(error) {
    message('WARNING: Encountered problems while loading stations. See error log and fix CRITICAL errors.',
            '\n You may need to upload a file of buoy deployment positions. Select a file now, \n',
            ' or cancel and re-run with argument buoyPositions set to missing deployment data. \n',
            ' Please ensure that the dateFormat argument matches your data (default %Y-%m-%d %H:%M:%S, see ?strptime for options).')
    try({
      buoyPositions <- file.choose()
      return(loadStations(folder=folder, db.ext=db.ext, buoyPositions = buoyPositions, ...))
    })
  }
  if(exists('st.list')) {
    names(st.list) <- basename(fnames)
    attr(st.list, "survey") <- folder
    st.list
  } else {
    stop('loadStations failed. See error log.')
  }
}