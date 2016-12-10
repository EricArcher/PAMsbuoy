labelDetection <- function(df) {
  grp <- df$MatchedAngles
  grp <- ifelse(is.na(grp), df$Id, grp)
  grp <- sapply(strsplit(grp, ";"), function(x) min(as.numeric(x)))
  grp <- factor(as.numeric(grp))
  levels(grp) <- 1:nlevels(grp)
  as.numeric(grp)
}

loadDifar <- function(db.fname) {
  con <- dbConnect(RSQLite::SQLite(), db.fname)
  difar <- dbReadTable(con, "DIFAR_Localisation")
  dbDisconnect(con)
  difar$TriggerName <- gsub(" ", "", difar$TriggerName)
  difar$Species <- gsub(" ", "", difar$Species)
  difar$MatchedAngles <- gsub(" ", "", difar$MatchedAngles)
  difar$TrackedGroup <- gsub(" ", "", difar$TrackedGroup)
  difar$detection <- labelDetection(difar)
  difar
}

clipBuoyLatLon <- function(df, lat.range = NULL, lon.range = NULL) {
  if(is.null(lat.range)) lat.range <- range(df$BuoyLatitude)
  if(is.null(lon.range)) lon.range <- range(df$BuoyLongitude)
  df <- df[!(is.na(df$BuoyLatitude) | is.na(df$BuoyLongitude)), ]
  lat.good <- isBetween(df$BuoyLatitude, lat.range, include.ends = TRUE)
  lon.good <- isBetween(df$BuoyLongitude, lon.range, include.ends = TRUE)
  df[lat.good & lon.good, ]
}

buoyLoc <- function(df) {
  result <- do.call(rbind, by(df, list(buoy = df$Channel), function(x) {
    c(
      latitude = median(x$BuoyLatitude, na.rm = TRUE),
      longitude = median(x$BuoyLongitude, na.rm = TRUE)
    )
  }))
  cbind(buoy = rownames(result), as.data.frame(result))
}
