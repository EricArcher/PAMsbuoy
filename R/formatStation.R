#' @name formatStation
#' @title Format Station Data
#' @description Format data for a station from an list of tables read from
#'   a PAMGuard SQLlite database.
#'
#' @param db list of data frames from PAMGuard SQLlite database
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom dplyr mutate select arrange filter rename bind_rows bind_cols
#' @importFrom magrittr %>%
#' @importFrom purrr transpose
#' @importFrom stringr str_trim
#' @importFrom tidyr spread
#' @export
#'
formatStation <- function(db) {
  position <- formatBuoyPosition(db)
  calibration <- formatBuoyCalibration(db, position)
  effort <- formatBuoyEffort(db)
  # transpose to list of position, calibration, and effort for each buoy
  buoys <- transpose(list(
    position = position,
    calibration = calibration,
    effort = effort
  ))

  # a list of each detection
  detections = formatDetections(db, buoys)
  st <- list(buoys = buoys, detections = detections)

  attr(st, "station") <- attr(db, "station")
  st
}

#' @rdname formatStation
#'
formatBuoyPosition <- function(db) {
  # a list of position data for each buoy
  db$HydrophoneStreamers %>%
    mutate(Buoy = as.character(StreamerIndex)) %>%
    select(Buoy, UTC, Latitude, Longitude) %>%
    arrange(Buoy, UTC) %>%
    split(., .$Buoy)
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
  eff <- db$Listening_Effort %>%
    filter(Status %in% c("on effort", "off effort")) %>%
    arrange(UTC) %>%
    select(UTC, Status)

  noise.off <- db$Spectrogram_Annotation %>%
    filter(Note == "noise") %>%
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

  buoys <- sort(unique(db$DIFAR_Localisation$Channel))

  final.effort <- sapply(buoys, function(b) {
    b.noise <- noise %>%
      filter(Channels == b) %>%
      select(-Channels)

    b.eff <- bind_rows(eff, b.noise) %>%
      arrange(UTC) %>%
      mutate(Buoy = b) %>%
      select(Buoy, UTC, Status)

    first.on <- min(which(b.eff$Status == "on effort"))
    b.eff <- b.eff[first.on:nrow(b.eff), ]

    good <- sapply(2:nrow(b.eff), function(i) {
      if(b.eff$Status[i] == "on effort") {
        b.eff$Status[i - 1] == "off effort"
      } else {
        b.eff$Status[i - 1] == "on effort"
      }
    })
    good <- c(TRUE, good)
    b.eff <- b.eff[good, ]

    last.off <- b.eff$Status[nrow(b.eff)] == "off effort"
    while(!last.off) {
      b.eff <- b.eff[-nrow(b.eff), ]
      last.off <- b.eff$Status[nrow(b.eff)] == "off effort"
    }

    b.eff %>%
      mutate(
        effort.id = rep(1:(n() / 2), each = 2),
        Status = gsub(" ", ".", Status)
      ) %>%
      spread(Status, UTC) %>%
      rename(on = on.effort, off = off.effort) %>%
      mutate(duration = difftime(off, on, units = "secs")) %>%
      select(Buoy, on, off, duration)
  }, simplify = FALSE)
  names(final.effort) <- buoys

  final.effort
}

#' @rdname formatStation
#'
formatDetections <- function(db, buoys) {
  detections <- db$DIFAR_Localisation %>%
    filter(Species != "vessel") %>%
    mutate(
      Buoy = as.character(Channel),
      MatchedAngles = gsub(" ", "", MatchedAngles)
    ) %>%
    mutate(detection = labelDetection(.)) %>%
    select(
      detection, Buoy, UTC, DIFARBearing, ClipLength, DifarFrequency,
      SignalAmplitude, DifarGain, Species
    ) %>%
    arrange(detection, Buoy, UTC)

  detectionEffortStatus <- function(b, dt, buoys) {
    eff <- buoys[[b]]$effort
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

  bind_cols(detections, effort.status) %>%
    split(., .$detection)
}
