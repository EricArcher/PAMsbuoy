#' @title Label Detections
#' @description Label common DIFAR detections.
#'
#' @param df data.frame of DIFAR data
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @export
#'
labelDetection <- function(df) {
  grp <- df$MatchedAngles
  grp <- ifelse(is.na(grp), df$Id, grp)
  grp <- sapply(strsplit(grp, ";"), function(x) min(as.numeric(x)))
  grp <- factor(as.numeric(grp))
  levels(grp) <- 1:nlevels(grp)
  as.numeric(grp)
}
