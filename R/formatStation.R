#' @name formatStation
#' @title Format Station Data
#' @description Format data for a station from an list of tables read from
#'   a PAMGuard SQLlite database.
#'
#' @param db list of data frames from PAMGuard SQLlite database
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom dplyr mutate select arrange filter rename bind_rows bind_cols select_
#' @importFrom magrittr %>%
#' @importFrom purrr transpose
#' @importFrom stringr str_trim
#' @importFrom tidyr spread
#' @importFrom stringdist stringsim
#' @export
#'
formatStation <- function(db, ...) {
  position <- formatBuoyPosition(db)
  calibration <- formatBuoyCalibration(db, position)
  effort <- formatBuoyEffort(db)

  missing.buoys <- setdiff(names(position), names(calibration))
  if(length(missing.buoys) > 0) {
    for(b in missing.buoys) calibration[b] <- list(NULL)
    calibration <- calibration[order(names(calibration))]
    message("  no calibration records for buoys ", paste(missing.buoys, collapse = ", "))
  }

  missing.buoys <- setdiff(names(position), names(effort))
  if(length(missing.buoys) > 0) {
    for(b in missing.buoys) effort[b] <- list(NULL)
    effort <- effort[order(names(effort))]
    message("  no effort records for buoys ", paste(missing.buoys, collapse = ", "))
  }

  # transpose to list of position, calibration, and effort for each buoy
  buoys <- purrr::transpose(list(
    position = position,
    calibration = calibration,
    effort = effort
  ))

  # a list of each detection
  detections <- formatDetections(db, buoys, ...)
  st <- list(buoys = buoys, detections = detections)

  attr(st, "station") <- attr(db, "station")
  st
}

#' @rdname formatStation
#'
formatBuoyPosition <- function(db) {
  # a list of position data for each buoy
  # db$HydrophoneStreamers %>%
  #   mutate(Buoy = as.character(StreamerIndex)) %>%
  #   # filter(DifarModuleAction=='deployed') %>%
  #   select(Buoy, UTC, Latitude, Longitude) %>%
  #   arrange(Buoy, UTC) %>%
  #   split(., .$Buoy)

  # Force to use new version where this column exists?
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
    split(., .$Buoy) %>%
    calculateOffset(position, db)
}

#' @rdname formatStation
#'
formatBuoyEffort <- function(db) {
  buoys <- sort(unique(db$DIFAR_Localisation$Channel))
  buoys <- sort(unique(db$HydrophoneStreamers$StreamerIndex))

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
        message(' last effort record for buoy ', b, ' is not "off effort"')
      }
      b.eff <- b.eff[1:max(offs),]
    }
    browser()
    # Want a clean way of doing this if nrow=1. Doesnt work with vector result, just giving false[1]

    b.eff %>%
      mutate(
        effort.id = ifelse(nrow(b.eff)==1, 1, rep(1:(n() / 2), each = 2)),
        Status = gsub(" ", ".", Status)
      ) %>%
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
