#' @name applyCalibration
#' @title Apply Bearing Calibration
#' @description Return calibrated DIFAR angles using different methods
#'
#' @param angle.data data.frame containing a \code{Buoy} column and a
#' \code{DIFARBearing} column with uncalibrated angle data
#' @param buoy.data list of data.frames containing buoy calibration data
#' @param method method of angle calibration to use. \code{simple} uses the median
#' of offset angles. \code{sine} uses sinusoidal fit to error.
#' @param \dots arguments passed on to other functions
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @export
#'
applyCalibration <- function(angle.data, buoy.data, method = c("simple", "sine"), ...) {
  switch(
    match.arg(method),
    simple = simpleAngleCalibration(angle.data, buoy.data)
  )
}

#' @rdname applyCalibration
#'
simpleAngleCalibration <- function(angle.data, buoy.data) {
  do.call(rbind,
          by(angle.data, angle.data$Buoy, function(x) {
            cal.value <- median(buoy.data[[x$Buoy[1]]]$calibration$offset)
            x$CalibrationValue <- cal.value
            x$CalibratedAngle <- x$DIFARBearing + cal.value
            x
          })
  )
}
