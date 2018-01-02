#' @title Label Detections
#' @description Label common DIFAR detections.
#'
#' @param df data.frame of DIFAR data
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
labelDetection <- function(df) {
  grp <- df$MatchedAngles
  id <- as.character(df$Id)
  i <- which(is.na(grp))
  # Include IDs into set of matched ids
  grp[-i] <- paste(id[-i], grp[-i], sep = ";")
  grp[i] <- id[i]
  # Extract ID numbers
  grp <- strsplit(grp, ";")
  names(grp) <- id
  # Check if all MatchedAngle ids are in Ids column
  if(!all(unlist(grp) %in% df$Id)) {
    missingIds <- unique(unlist(grp)[which(!(unlist(grp) %in% df$Id))])
    badRowIds <- which(sapply(grp, function(x) any(x %in% missingIds)))
    message('  Id(s) ', paste(missingIds, collapse=', '), ' are in the MatchedAngles ',
            'column but not the Id column. \n',
             '  Check rows with Id(s) ', paste(badRowIds, collapse=', '),
            ' in the DIFAR_Localisation table.')
    # Just remove them for now?
    for(i in badRowIds) {
      grp[[i]] <- grp[[i]][which(!(grp[[i]] %in% missingIds))]
    }
  }
  # replaced matched ids with intersections of IDs
  for(i in id) {
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
