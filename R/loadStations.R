#' @title Load Multiple Stations
#' @description Load a list of sonobuoy stations from SQLlite databases in a folder
#'
#' @param folder folder containing SQLite databases with DIFAR data. if missing
#'   a dialog box will be presented to choose a folder
#' @param dbExt database extension to look for in chosen folder
#' @param \dots other parameters to be passed to formatStation function
#'
#' @return a list of sonobuoy stations
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom tcltk tk_choose.dir
#' @importFrom tools list_files_with_exts
#' @export
#'
loadStations <- function(folder, dbExt = "sqlite3", ...) {
  if(missing(folder)) folder <- tcltk::tk_choose.dir()
  if(is.na(folder)) stop("No folder chosen")
  # If only 1 file, not a folder
  if(grepl(paste0(dbExt, '$'), folder)) {
    if(!file.exists(folder)) {
      stop('File ', folder, ' does not exist.')
    }
    fnames <- folder
  } else {
    fnames <- tools::list_files_with_exts(folder, exts = dbExt)
    fnames <- fnames[order(nchar(fnames), fnames)]
  }

  logFname <- "PAMsbuoy_loadStations_log.txt"
  log <- file(logFname, open = "wt")
  sink(log, type = "message")

  error <- FALSE
  # need to wrap this in a try, otherwise if theres an error we dont close the sink connection
  cat('Loading stations... \n')
  pb <- txtProgressBar(min=0, max=length(fnames), style=3)
  stationList <- vector('list', length=length(fnames))
  names(stationList) <- basename(fnames)
  for(f in seq_along(fnames)) {
    message('\n', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ":", fnames[f])
    tryCatch({
      station <- formatStation(loadDB(fnames[f], FALSE), overrideError = TRUE, ...)
      for(b in seq_along(station$buoys)) {
        if(station$buoys[[b]]$error) {
          error <<- TRUE
        }
        station$buoys[[b]] <- station$buoys[[b]][!(names(station$buoys[[b]]) == 'error')]
      }
      stationList[[f]] <- station
    }, error = function(e) {
      message(e, '\n')
      error <<- TRUE
    })
    setTxtProgressBar(pb, f)
  }
  sink(type = "message")
  file.show(logFname)
  if(error) {
    message('\nWARNING: Encountered problems while loading stations. See error log and fix CRITICAL errors.')
  }
  if(exists('stationList')) {
    stationList <- stationList[sapply(stationList, function(x) !is.null(x))]
    attr(stationList, "survey") <- folder
    stationList
  } else {
    stop('loadStations failed. See error log.')
  }
}
