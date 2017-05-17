#' Modules to return buoy location based on time from
#'     1) initial location
#'     2) imported GPS data
#'     3) drift calculation (with uncertainty)
#' Calibration:
#'     1) extract "Vessel" species from 'DIFAR_localisation' table
#'     2) GPS location from 'gpsData' table - closest in time
#'     3) store distribution of angle differences for each buoy
#' Distribution of bearing angles for each detection by each buoy


#' TODO: Buoy Summary

#' TODO: Detection Summary

#' TODO: "Station" Summary (set of buoys)
#'  - Total effort minus noise in window for a station
