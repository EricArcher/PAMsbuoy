#' @name formatStation
#' @title Format Station Data
#' @description Format data for a station from an list of tables read from
#'   a PAMGuard SQLlite database.
#'
#' @param db list of data frames from PAMGuard SQLlite database
#' @param buoyPositions optional argument, can be a data frame containing position
#'   information for buoy deployments or a file path. Data frame must have columns \code{Buoy},
#'   \code{UTC}, \code{Latitude}, and \code{Longitude}. If buoy positions are present
#'   both in the database and this dataframe, positions from this data frame will be used.
#'   File path must be to a csv file to be read in as a data frame.
#' @param overrideError station formatting will stop if insufficient buoy position data is
#'   present. Set this to \code{TRUE} to override this behaviour and continue loading
#'   the station and mark problematic buoys with \code{error=TRUE}.
#' @param dateFormat character string giving date-time format as used by strptime. This
#'   is used to format the dates of data supplied by buoyPositions.
#' @param extraCols character vector listing any extra columns to keep while loading
#'   detection data.
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}, Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom dplyr mutate select arrange filter rename bind_rows bind_cols select_
#' @importFrom magrittr %>%
#' @importFrom purrr transpose
#' @importFrom stringr str_trim str_replace_all
#' @importFrom tidyr spread
#' @importFrom stringdist stringsim
#' @export
#'
formatStation <- function(db, buoyPositions = NULL, overrideError=FALSE,
                          dateFormat = '%Y-%m-%d %H:%M:%S', extraCols = NULL, ...) {
  db$DIFAR_Localisation$Species <- tolower(str_trim(db$DIFAR_Localisation$Species))
  dbPositions <- formatBuoyPosition(db)
  # buoyPositions either not supplied, supplied as df, or as file path
  if(is.character(buoyPositions)) {
    if(!file.exists(buoyPositions)) {
      stop('  File ', buoyPositions, ' does not exist.')
    }
    buoyPositions <- read.csv(buoyPositions, stringsAsFactors = FALSE)
  }
  # Checking for proper formatting
  if(is.data.frame(buoyPositions)) {
    neededCols <- c('Buoy', 'UTC', 'Latitude', 'Longitude')
    if(!all(neededCols %in% colnames(buoyPositions))) {
      stop('Provided buoyPositions file must have columns ', paste(neededCols, collapse=', '))
    }
    buoyPositions$UTC <- as.POSIXct(buoyPositions$UTC, tz='GMT', format=dateFormat)
    if('Station' %in% colnames(buoyPositions)) {
      buoyPositions <- mutate(buoyPositions, Station = gsub('\\..*$', '', Station)) %>%
        filter(Station == gsub('\\..*$', '', attr(db, 'station')))
    }
    buoyPositions <- select(buoyPositions, Buoy, UTC, Latitude, Longitude) %>%
      arrange(Buoy, UTC) %>%
      split(., .$Buoy)
  }
  # Make position list favoring user defined positions over db positions
  position <- dbPositions
  for(b in names(buoyPositions)) {
    position[[b]] <- buoyPositions[[b]]
  }
  calibration <- formatBuoyCalibration(db, position)
  effort <- formatBuoyEffort(db)

  difarNames <- unique(db$DIFAR_Localisation$Channel)
  buoyList <- unique(c(names(position), names(calibration), names(effort), difarNames))

  # If somehow there is no information, just return NULL with a big warning
  if(length(buoyList) == 0) {
    message('**CRITICAL: There are no position, effort, calibration, or DIFAR Localisation records')
    st <- NULL
    attr(st, 'station') <- attr(db, 'station')
    return(st)
  }
  error <- list()
  for(b in buoyList) {
    error[b] <- FALSE
  }
  names(error) <- buoyList

  info <- formatBuoyInfo(buoyList)

  missingPositions <- setdiff(buoyList, names(position))
  if(length(missingPositions) > 0) {
    message('**CRITICAL: Missing deployment position information for buoy(s) ',
            paste(missingPositions, collapse = ', '), '. \n',
            '** You must fix in the database or provide as a separate csv file.\n',
            'If providing a csv file, it must have columns UTC, Buoy, Latitude, Longitude. ',
            'Dates should be in YYYY-MM-DD H:M:S format (this can be changed with the dateFormat',
            'argument.')
    if(!overrideError) {
      buoyPositions <- file.choose()
      return(formatStation(db = db, buoyPositions = buoyPositions, overrideError = overrideError,
                           dateFormat = dateFormat, extraCols = extraCols, ...))
    } else {
      for(b in missingPositions) {
        position[b] <- list(NULL)
        error[b] <- TRUE
      }
    }
  }
  position <- position[order(names(position))]
  error <- error[order(names(error))]

  missingCalibration <- setdiff(buoyList, names(calibration))
  if(length(missingCalibration) > 0) {
    for(b in missingCalibration) {
      calibration[b] <- list(NULL)
    }
    calibration <- calibration[order(names(calibration))]
    message("  no calibration records for buoy(s) ", paste(missingCalibration, collapse = ", "))
  }

  missingEffort <- setdiff(buoyList, names(effort))
  if(length(missingEffort) > 0) {
    for(b in missingEffort) {
      effort[b] <- list(NULL)
    }
    effort <- effort[order(names(effort))]
    message("  no complete effort records for buoy(s) ", paste(missingEffort, collapse = ", "))
  }

  calibration <- calculateOffset(calibration, position, db)
  # transpose to list of position, calibration, and effort for each buoy
  buoyTemp <- list(
    position = position,
    calibration = calibration,
    effort = effort,
    info = info)
  # Dont really want to carry this around if we dont have to. Used for checking in loadStations
  if(overrideError) {
    buoyTemp$error <- error
  }
  buoys <- purrr::transpose(buoyTemp)

  # a list of each detection
  detections <- formatDetections(db, buoys, extraCols)
  missingDetections <- setdiff(unique(detections$Buoy), buoyList)
  if(length(missingDetections) > 0) {
    message('**CRITICAL: Detections are present for buoy(s)',
            paste(missingDetections, collapse = ', '),
            ', but there is no deployment, effort, or calibration data. \n',
            '** You must fix in the database or provide deployment position in a separate csv file.')
    if(!overrideError) {
      buoyPositions <- file.choose()
      return(formatStation(db = db, buoyPositions = buoyPositions, overrideError = overrideError,
                           dateFormat = dateFormat, extraCols = extraCols, ...))
    } else {
      for(b in missingDetections) {
        buoys[[b]] <- list(error=TRUE)
      }
      buoys <- buoys[order(names(buoys))]
    }
  }

  # extra station info
  stationInfo <- formatStationInfo(db)

  st <- list(buoys = buoys, detections = detections, stationInfo = stationInfo)

  attr(st, "station") <- attr(db, "station")
  st
}

#' @rdname formatStation
#'
formatBuoyPosition <- function(db) {
  # a list of position data for each buoy
  # Checks for deployed in DifarModuleAction column.
  # This should be present in all PG versions >= 1.15.
  df <- db$HydrophoneStreamers
  if('DifarModuleAction' %in% colnames(df)) {
    df %>%
      mutate(Buoy = as.character(StreamerIndex),
             DifarModuleAction = str_trim(DifarModuleAction)) %>%
      filter(DifarModuleAction == 'deployed') %>%
      select(Buoy, UTC, Latitude, Longitude) %>%
      arrange(Buoy, UTC) %>%
      split(., .$Buoy)
  }
}

#' @rdname formatStation
#'
formatBuoyCalibration <- function(db, position) {
  # a list of calibration data for each buoy
  db$DIFAR_Localisation %>%
    filter(Species == "vessel") %>%
    mutate(Buoy = as.character(Channel)) %>%
    select(Buoy, UTC, DIFARBearing) %>%
    arrange(Buoy, UTC) %>%
    split(., .$Buoy) # %>%
    # calculateOffset(position, db)
}

#' @rdname formatStation
#'
formatBuoyEffort <- function(db) {
  buoys <- sort(unique(db$DIFAR_Localisation$Channel))
  buoys <- union(buoys, sort(unique(db$HydrophoneStreamers$StreamerIndex)))

  # Check if we have Spectrogram and Effort tables, if we dont make a blank
  if(!('Spectrogram_Annotation' %in% names(db))) {
    db$Spectrogram_Annotation <- data.frame(UTC=numeric(),
                                            Channels=integer(),
                                            Note=character(),
                                            Duration=numeric())
  }
  if(!('Listening_Effort' %in% names(db))) {
    db$Listening_Effort <- data.frame(UTC=numeric(),
                                      Channels=integer(),
                                      Status=character())
  }
  # identify noise Notes
  db$Spectrogram_Annotation <- db$Spectrogram_Annotation %>%
    mutate(
      Note = tolower(str_trim(Note)),
      noiseSim = stringsim(Note, "noise"),
      isNoise = noiseSim >= 0.8 | grepl("noise", Note)
    )
  i <- with(db$Spectrogram_Annotation, which(!isNoise & Note != ""))
  if(length(i) > 0) {
    message(
      "  Spectrogram_Annotation records ",
      paste(i, collapse = ", "),
      " may have misspelled 'noise' Notes"
    )
  }

  noiseOff <- db$Spectrogram_Annotation %>%
    filter(isNoise) %>%
    mutate(Status = "off effort") %>%
    select(Channels, UTC, Duration, Status)
  noiseOn <- noiseOff %>%
    mutate(
      UTC = UTC + Duration,
      Status = "on effort"
    )
  noise <- bind_rows(noiseOff, noiseOn) %>%
    arrange(Channels, UTC) %>%
    select(-Duration)

  eff <- db$Listening_Effort %>%
    mutate(Status = tolower(str_trim(Status))) %>%
    filter(Status %in% c("on effort", "off effort")) %>%
    arrange(UTC) %>%
    select(UTC, Status, Channels)

  finalEffort <- sapply(buoys, function(b) {
    buoyNoise <- noise
    buoyNoise$isBuoy <- sapply(buoyNoise$Channels, function(x) {
      any(convertBinaryChannel(x) == b)
    })
    buoyNoise <- buoyNoise %>%
      filter(isBuoy) %>%
      select(-Channels, -isBuoy)

    buoyEff <- eff %>%
      mutate(isBuoy = sapply(Channels, function(x) {
        any(convertBinaryChannel(x) == b)
      })) %>%
      filter(isBuoy) %>%
      select(-Channels, -isBuoy)

    if(nrow(buoyEff)==0) {
      return(NULL)
    }

    if(nrow(buoyEff)==1) {
      message('  buoy ', b, ' has only one effort record')
    }

    buoyEff <- bind_rows(buoyEff, buoyNoise) %>%
      arrange(UTC) %>%
      mutate(Buoy = b) %>%
      select(Buoy, UTC, Status)

    ons <- which(buoyEff$Status == "on effort")
    if(length(ons)==0) {
      message('  no "on effort" records for buoy ', b)
      return(NULL)
    } else {
      if(min(ons) != 1) {
        message("  first effort record for buoy ", b, " is not 'on effort'")
      }
      buoyEff <- buoyEff[min(ons):nrow(buoyEff), ]
    }

    if(nrow(buoyEff)==1) {
      good <- TRUE
    } else {
      good <- sapply(2:nrow(buoyEff), function(i) {
        if(buoyEff$Status[i] == "on effort") {
          buoyEff$Status[i - 1] == "off effort"
        } else {
          buoyEff$Status[i - 1] == "on effort"
        }
      })
      good <- c(TRUE, good)
    }
    if(any(!good)) {
      message("  on and off effort records for buoy ", b, " are not alternating")
    }
    buoyEff <- buoyEff[good, ]

    offs <- which(buoyEff$Status == 'off effort')
    if(length(offs)==0) {
      message('  no "off effort" records for buoy ', b)
      return(NULL)
    } else {
      if(max(offs) != nrow(buoyEff)) {
        message('  last effort record for buoy ', b, ' is not "off effort"')
      }
      buoyEff <- buoyEff[1:max(offs),]
    }

    # Want a clean way of doing this if nrow=1. Doesnt work with vector result, just giving false[1]
    if(nrow(buoyEff)==1) {
      buoyEff <- buoyEff %>% mutate(
        effortId = 1,
        Status = gsub(' ', '.', Status)
    )} else {
      buoyEff <- buoyEff %>% mutate(
        effortId = rep(1:(n()/2), each=2),
        Status = gsub(' ', '.', Status)
    )}

    buoyEff %>%
      # mutate(
      #   effort.id = ifelse(nrow(b.eff)==1, 1, rep(1:(n() / 2), each = 2)),
      #   Status = gsub(" ", ".", Status)
      # ) %>%
      spread(Status, UTC) %>%
      rename(on = on.effort, off = off.effort) %>%
      mutate(duration = difftime(off, on, units = "secs")) %>%
      select(Buoy, on, off, duration)
  }, simplify = FALSE)

  names(finalEffort) <- buoys

  Filter(Negate(is.null), finalEffort)
}

#' @rdname formatStation
#'
formatDetections <- function(db, buoys, extraCols = NULL) {
  keepCols <- c('detection', 'Buoy', 'UTC', 'DIFARBearing', 'ClipLength',
                'DifarFrequency', 'SignalAmplitude', 'DifarGain', 'Species',
                'calibrationValue', 'calibratedBearing')
  if(!is.null(extraCols) & all(extraCols %in% colnames(db$DIFAR_Localisation))) {
    keepCols <- c(keepCols, extraCols)
  }
  detections <- db$DIFAR_Localisation %>%
    filter(Species != "vessel") %>%
    mutate(
      Buoy = as.character(Channel),
      MatchedAngles = gsub(" ", "", MatchedAngles)
    ) %>%
    mutate(detection = labelDetection(.),
           calibrationValue = NA,
           calibratedBearing = NA) %>%
    select_(.dots = keepCols) %>%
    arrange(detection, Buoy, UTC)

  detectionEffortStatus <- function(b, dt, buoys) {
    eff <- buoys[[b]]$effort
    if(is.null(eff)) return(
      data.frame(
        effortWindow = NA,
        effort = NA,
        effortDuration = NA,
        stringsAsFactors = FALSE
      )
    )
    for(i in 1:nrow(eff)) {
      if(eff$on[i] <= dt & eff$off[i] >= dt) {
        return(data.frame(
          effortWindow = i,
          effort = "on",
          effortDuration = difftime(dt, eff$on[i], units = "secs"),
          stringsAsFactors = FALSE
        ))
      }
    }
    data.frame(
      effortWindow = NA,
      effort = "off",
      effortDuration = NA,
      stringsAsFactors = FALSE
    )
  }

  effortStatus <- mapply(
    detectionEffortStatus,
    b = detections$Buoy,
    dt = detections$UTC,
    MoreArgs = list(buoys = buoys),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ) %>% bind_rows

  bind_cols(detections, effortStatus)
}

formatStationInfo <- function(db) {
  # Rename station_type from DB values to words
  rename <- c('de'='DensityEstimate',
              'opp'='Opportunistic')
  deployInfo <- db$Deploy
  if(is.null(deployInfo) || (nrow(deployInfo) == 0)) {
    message('  no station metadata found in the "Deploy" table')
    # Fill with blanks so we can see this in summaries and such
    deployInfo <- data.frame(cruise=NA, instrument_type=NA,
                              instrument_id=NA, station_type='DensityEstimate', vis_id=NA)
  } else {
    deployInfo <- deployInfo %>%
      mutate(cruise = str_trim(cruise),
             instrument_type = tolower(str_trim(instrument_type)),
             instrument_id = str_trim(instrument_id),
             station_type = tolower(str_trim(station_type)),
             station_type = str_replace_all(station_type, rename),
             vis_id = str_trim(vis_id)) %>%
      select(cruise, instrument_type, instrument_id, station_type, vis_id)
  }
  soundInfo <- db$Sound_Acquisition %>%
    arrange(desc(duration)) %>%
    slice(1)
  if(is.null(soundInfo) || (nrow(soundInfo) == 0)) {
    message('  no sound recording data found in the "Sound_Acquisition" table')
    soundInfo <- data.frame(sampleRate = NA,
                            recordingLength = NA,
                            recordingSystem = NA)
  } else {
    soundInfo <- soundInfo %>%
      mutate(recordingLength = duration,
             recordingSystem = str_trim(SystemType)) %>%
      select(sampleRate, recordingLength, recordingSystem)
  }
  cbind(deployInfo, soundInfo)
}

formatBuoyInfo <- function(buoyList) {
  info <- vector('list', length=length(buoyList))
  for(b in seq_along(buoyList)) {
    info[[b]] <- list(buoyQuality = NA,
                    calibrationType = NA,
                    drift = list(rate = NA, bearing = NA, stderr = NA,
                                 errorPlot = NA, mapPlot = NA, quality = NA))
  }
  names(info) <- buoyList
  info
}
#' @rdname formatStation
#' @keywords internal
#'
convertBinaryChannel <- function(x) {
  x <- as.integer(intToBits(x))[1:4]
  which(x == 1) - 1
}
