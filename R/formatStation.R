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
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}, Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @importFrom dplyr mutate select arrange filter rename bind_rows bind_cols select_
#' @importFrom magrittr %>%
#' @importFrom purrr transpose
#' @importFrom stringr str_trim
#' @importFrom tidyr spread
#' @importFrom stringdist stringsim
#' @export
#'
formatStation <- function(db, buoyPositions = NULL, overrideError=FALSE,
                          dateFormat = '%Y-%m-%d %H:%M:%S', ...) {
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

  buoyList <- unique(c(names(position), names(calibration), names(effort)))
  # if(length(buoyList) == 0) {
  #   stop(
  error <- list()
  for(b in buoyList) {
    error[b] <- FALSE
  }
  names(error) <- buoyList

  missing.positions <- setdiff(buoyList, names(position))
  if(length(missing.positions) > 0) {
    message('**CRITICAL: Missing deployment position information for buoy(s) ',
            paste(missing.positions, collapse = ', '), '. \n',
            '** You must fix in the database or provide as a separate csv file.')
    if(!overrideError) {
      buoyPositions <- file.choose()
      return(formatStation(db = db, buoyPositions = buoyPositions,
                           overrideError = overrideError, dateFormat = dateFormat, ...))
    } else {
      for(b in missing.positions) {
        position[b] <- list(NULL)
        error[b] <- TRUE
      }
    }
  }
  position <- position[order(names(position))]
  error <- error[order(names(error))]

  missing.calibration <- setdiff(buoyList, names(calibration))
  if(length(missing.calibration) > 0) {
    for(b in missing.calibration) {
      calibration[b] <- list(NULL)
    }
    calibration <- calibration[order(names(calibration))]
    message("  no calibration records for buoy(s) ", paste(missing.calibration, collapse = ", "))
  }

  missing.effort <- setdiff(buoyList, names(effort))
  if(length(missing.effort) > 0) {
    for(b in missing.effort) {
      effort[b] <- list(NULL)
    }
    effort <- effort[order(names(effort))]
    message("  no effort records for buoy(s) ", paste(missing.effort, collapse = ", "))
  }

  calibration <- calculateOffset(calibration, position, db)
  # transpose to list of position, calibration, and effort for each buoy
  buoyTemp <- list(
    position = position,
    calibration = calibration,
    effort = effort)
  # Dont really want to carry this around if we dont have to. Used for checking in loadStations
  if(overrideError) {
    buoyTemp$error <- error
  }
  buoys <- purrr::transpose(buoyTemp)

  # a list of each detection
  detections <- formatDetections(db, buoys, ...)
  missing.detections <- setdiff(unique(detections$Buoy), buoyList)
  if(length(missing.detections) > 0) {
    message('**CRITICAL: Detections are present for buoy(s)',
            paste(missing.detections, collapse = ', '),
            ', but there is no deployment, effort, or calibration data. \n',
            '** You must fix in the database or provide deployment position in a separate csv file.')
    if(!overrideError) {
      buoyPositions <- file.choose()
      return(formatStation(db = db, buoyPositions = buoyPositions,
                           overrideError = overrideError, dateFormat = dateFormat, ...))
    } else {
      for(b in missing.detections) {
        buoys[[b]] <- list(error=TRUE)
      }
      buoys <- buoys[order(names(buoys))]
    }
  }
  st <- list(buoys = buoys, detections = detections)

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

  # identify noise Notes
  db$Spectrogram_Annotation <- db$Spectrogram_Annotation %>%
    mutate(
      Note = tolower(str_trim(Note)),
      noise.sim = stringsim(Note, "noise"),
      is.noise = noise.sim >= 0.8 | grepl("noise", Note)
    )
  i <- with(db$Spectrogram_Annotation, which(!is.noise & Note != ""))
  if(length(i) > 0) {
    message(
      "  Spectrogram_Annotation records ",
      paste(i, collapse = ", "),
      " may have misspelled 'noise' Notes"
    )
  }

  noise.off <- db$Spectrogram_Annotation %>%
    filter(is.noise) %>%
    mutate(Status = "off effort") %>%
    select(Channels, UTC, Duration, Status)
  noise.on <- noise.off %>%
    mutate(
      UTC = UTC + Duration,
      Status = "on effort"
    )
  noise <- bind_rows(noise.off, noise.on) %>%
    arrange(Channels, UTC) %>%
    select(-Duration)

  eff <- db$Listening_Effort %>%
    filter(Status %in% c("on effort", "off effort")) %>%
    arrange(UTC) %>%
    select(UTC, Status, Channels)

  final.effort <- sapply(buoys, function(b) {
    b.noise <- noise
    b.noise$is.buoy <- sapply(b.noise$Channels, function(x) {
      any(convertBinaryChannel(x) == b)
    })
    b.noise <- b.noise %>%
      filter(is.buoy) %>%
      select(-Channels, -is.buoy)

    b.eff <- eff %>%
      mutate(is.buoy = sapply(Channels, function(x) {
        any(convertBinaryChannel(x) == b)
      })) %>%
      filter(is.buoy) %>%
      select(-Channels, -is.buoy)

    if(nrow(b.eff)==0) {
      return(NULL)
    }

    if(nrow(b.eff)==1) {
      message('  buoy ', b, ' has only one effort record')
    }

    b.eff <- bind_rows(b.eff, b.noise) %>%
      arrange(UTC) %>%
      mutate(Buoy = b) %>%
      select(Buoy, UTC, Status)

    ons <- which(b.eff$Status == "on effort")
    if(length(ons)==0) {
      message('  no "on effort" records for buoy ', b)
    } else {
      if(min(ons) != 1) {
        message("  first effort record for buoy ", b, " is not 'on effort'")
      }
      b.eff <- b.eff[min(ons):nrow(b.eff), ]
    }

    if(nrow(b.eff)==1) {
      good <- TRUE
    } else {
      good <- sapply(2:nrow(b.eff), function(i) {
        if(b.eff$Status[i] == "on effort") {
          b.eff$Status[i - 1] == "off effort"
        } else {
          b.eff$Status[i - 1] == "on effort"
        }
      })
      good <- c(TRUE, good)
    }
    if(any(!good)) {
      message("  on and off effort records for buoy ", b, " are not alternating")
    }
    b.eff <- b.eff[good, ]

    offs <- which(b.eff$Status == 'off effort')
    if(length(offs)==0) {
      message('  no "off effort" records for buoy ', b)
    } else {
      if(max(offs) != nrow(b.eff)) {
        message('  last effort record for buoy ', b, ' is not "off effort"')
      }
      b.eff <- b.eff[1:max(offs),]
    }

    # Want a clean way of doing this if nrow=1. Doesnt work with vector result, just giving false[1]
    if(nrow(b.eff)==1) {
      b.eff <- b.eff %>% mutate(
        effort.id = 1,
        Status = gsub(' ', '.', Status)
    )} else {
      b.eff <- b.eff %>% mutate(
        effort.id = rep(1:(n()/2), each=2),
        Status = gsub(' ', '.', Status)
    )}

    b.eff %>%
      # mutate(
      #   effort.id = ifelse(nrow(b.eff)==1, 1, rep(1:(n() / 2), each = 2)),
      #   Status = gsub(" ", ".", Status)
      # ) %>%
      spread(Status, UTC) %>%
      rename(on = on.effort, off = off.effort) %>%
      mutate(duration = difftime(off, on, units = "secs")) %>%
      select(Buoy, on, off, duration)
  }, simplify = FALSE)

  names(final.effort) <- buoys

  Filter(Negate(is.null), final.effort)
}

#' @rdname formatStation
#'
formatDetections <- function(db, buoys, extraCols = NULL) {
  keepCols <- c('detection', 'Buoy', 'UTC', 'DIFARBearing', 'ClipLength',
                'DifarFrequency', 'SignalAmplitude', 'DifarGain', 'Species')
  if(!is.null(extraCols) & all(extraCols %in% colnames(db$DIFAR_Localisation))) {
    keepCols <- c(keepCols, extraCols)
  }
  detections <- db$DIFAR_Localisation %>%
    filter(Species != "vessel") %>%
    mutate(
      Buoy = as.character(Channel),
      MatchedAngles = gsub(" ", "", MatchedAngles)
    ) %>%
    mutate(detection = labelDetection(.)) %>%
    select_(.dots = keepCols) %>%
    arrange(detection, Buoy, UTC)

  detectionEffortStatus <- function(b, dt, buoys) {
    eff <- buoys[[b]]$effort
    if(is.null(eff)) return(
      data.frame(
        effort.window = NA,
        effort = NA,
        effort.duration = NA,
        stringsAsFactors = FALSE
      )
    )
    for(i in 1:nrow(eff)) {
      if(eff$on[i] <= dt & eff$off[i] >= dt) {
        return(data.frame(
          effort.window = i,
          effort = "on",
          effort.duration = difftime(dt, eff$on[i], units = "secs"),
          stringsAsFactors = FALSE
        ))
      }
    }
    data.frame(
      effort.window = NA,
      effort = "off",
      effort.duration = NA,
      stringsAsFactors = FALSE
    )
  }

  effort.status <- mapply(
    detectionEffortStatus,
    b = detections$Buoy,
    dt = detections$UTC,
    MoreArgs = list(buoys = buoys),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ) %>% bind_rows

  bind_cols(detections, effort.status)
}

#' @rdname formatStation
#' @keywords internal
#'
convertBinaryChannel <- function(x) {
  x <- as.integer(intToBits(x))[1:4]
  which(x == 1) - 1
}
