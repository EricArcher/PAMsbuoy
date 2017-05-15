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
  i <- which(is.na(grp))
  # Include IDs into set of matched ids
  grp[-i] <- paste(df$Id[-i], grp[-i], sep = ";")
  grp[i] <- df$Id[i]
  # Extract ID numbers
  grp <- lapply(strsplit(grp, ";"), as.numeric)
  # replaced matched ids with intersections of IDs
  for(i in 1:nrow(df)) {
    matched.i <- grp[[i]]
    grp.i <- sort(unique(unlist(grp[matched.i])))
    matched.i <- sort(unique(c(matched.i, grp.i)))
    for(j in matched.i) grp[[j]] <- sort(unique(c(matched.i, grp[[j]])))
  }
  # create factor of matched ids and return group numbers
  grp <- sapply(grp, paste, collapse = ".")
  grp <- factor(grp, levels = unique(grp))
  as.numeric(grp)
}
