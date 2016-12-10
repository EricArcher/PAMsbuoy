readStationFile <- function(f) {
  df <- read.csv(f, na.strings = c("", " ", "NA"), stringsAsFactors = FALSE)
  if(nrow(df) == 0) return(NULL)
  dt <- paste0(df$UTC, ".", df$UTCMilliseconds)
  df$file <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(f))
  df$station <- gsub("_P[[:alnum:]]*", "", df$file)
  df$datetime <- strptime(dt, format = "%m/%d/%Y %H:%M:%OS", tz = "GMT")
  df
}


# --- DIFAR ---

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


# --- EFFORT ---

loadEffort <- function(folder) {
  fnames <- dir(folder, full.names = TRUE)
  df <- do.call(rbind, lapply(fnames, readStationFile))

  df <- df[df$UpdateOf != 0, c("UTC", "UTCMilliseconds", "Comment", "station", "file", "datetime")]
  df$Comment <- sub("[[:space:]]+$", "\\1", df$Comment)
  df <- df[order(df$station, df$datetime), ]
  paired.effort <- sapply(1:nrow(df), function(i) {
    if(df$Comment[i] == "START EFFORT") return(df$Comment[i + 1] == "END EFFORT")
    if(i > 1 & df$Comment[i] == "END EFFORT") return(df$Comment[i - 1] == "START EFFORT")
    FALSE
  })
  df[paired.effort, ]
}

effortPairs <- function(df) {
  eff.df <- do.call(rbind, lapply(seq(1, nrow(df), by = 2), function(i) {
    start <- df$datetime[i]
    end <- df$datetime[i + 1]
    data.frame(
      file = df$file[i], station = df$station[i],
      start = start, end = end,
      interval = difftime(end, start, units = "secs")
    )
  }))

  eff.df <- do.call(rbind, by(eff.df, eff.df$station, function(x) {
    data.frame(
      start = min(x$start),
      end = max(x$end),
      duration = sum(x$interval)
    )
  }))

  cbind(station = rownames(eff.df), eff.df)
}


# -- NOISE --

loadNoise <- function(folder) {
  fnames <- dir(folder, full.names = TRUE)
  df <- do.call(rbind, lapply(fnames, readStationFile))
  df$notes <- sub("[[:space:]]+$", "\\1", df$notes)
  has.noise <- grep("noise", df$notes, ignore.case = TRUE)
  if(length(has.noise) == 0) return(NULL)
  df[has.noise, ]
}
