#' @name applyCalibration
#' @title Apply Bearing Calibration
#' @description Return calibrated DIFAR angles using different methods
#'
#' @param angle.data data.frame containing a \code{Buoy} column and a
#' \code{DIFARBearing} column with uncalibrated angle data
#' @param buoy.data list of data.frames containing buoy calibration data
#' @param method method of angle calibration to use. \code{magnetic} uses
#' the median of magnetic varation values. \code{median} uses the median
#' of offset angles. \code{sine} uses sinusoidal fit to error.
#' @param \dots arguments passed on to other functions
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
applyCalibration <- function(angle.data, buoy.data, method = c("magnetic", "median", "sine"), ...) {
  switch(
    match.arg(method),
    magnetic = magneticAngleCalibration(angle.data, buoy.data),
    median = medianAngleCalibration(angle.data, buoy.data)
  )
}

#' @rdname applyCalibration
#'
magneticAngleCalibration <- function(angle.data, buoy.data) {
  do.call(rbind,
          by(angle.data, angle.data$Buoy, function(x) {
            cal.value <- median(buoy.data[[x$Buoy[1]]]$calibration$magnetic.variation)
            x$CalibrationValue <- cal.value
            x$CalibratedAngle <- x$DIFARBearing + cal.value
            x
          })
  )
}

#' @rdname applyCalibration
#'
medianAngleCalibration <- function(angle.data, buoy.data) {
  do.call(rbind,
          by(angle.data, angle.data$Buoy, function(x) {
            cal.value <- median(buoy.data[[x$Buoy[1]]]$calibration$offset)
            x$CalibrationValue <- cal.value
            x$CalibratedAngle <- x$DIFARBearing + cal.value
            x
          })
  )
}
