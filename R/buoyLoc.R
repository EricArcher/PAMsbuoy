#' @title Buoy Location
#' @description Get the median location of each buoy.
#'
#' @param df data.frame of DIFAR data
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @export
#'
buoyLoc <- function(df) {
  result <- do.call(rbind, by(df, list(buoy = df$Channel), function(x) {
    c(
      latitude = median(x$BuoyLatitude, na.rm = TRUE),
      longitude = median(x$BuoyLongitude, na.rm = TRUE)
    )
  }))
  cbind(buoy = rownames(result), as.data.frame(result))
}
