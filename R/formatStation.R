#' @title Format Station Data
#' @description Format data for a station from an list of tables read from
#'   a PAMGuard SQLlite database.
#'
#' @param db list of data frames from PAMGuard SQLlite database
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom dplyr mutate select arrange filter
#' @importFrom purrr transpose
#' @importFrom stringr str_trim
#' @export
#'
formatStation <- function(db) {
  # a list of position data for each buoy
  position <- db$HydrophoneStreamers %>%
    mutate(Buoy = as.character(StreamerIndex)) %>%
    select(Buoy, UTC, Latitude, Longitude) %>%
    arrange(Buoy, UTC) %>%
    split(., .$Buoy)

  # a list of calibration data for each buoy
  calibration <- db$DIFAR_Localisation %>%
    filter(Species == "vessel") %>%
    mutate(Buoy = as.character(Channel)) %>%
    select(Buoy, UTC, DIFARBearing) %>%
    arrange(Buoy, UTC) %>%
    split(., .$Buoy) %>%
    calculateOffset(position, db)

  # a list of each detection
  detections <- db$DIFAR_Localisation %>%
    filter(Species != "vessel") %>%
    mutate(
      Buoy = as.character(Channel),
      MatchedAngles = gsub(" ", "", MatchedAngles)
    ) %>%
    mutate(detection = labelDetection(.)) %>%
    select(
      detection, Buoy, UTC, DIFARBearing, ClipLength, DifarFrequency,
      SignalAmplitude, DifarGain
    ) %>%
    split(., .$detection)

  effort <- db$Listening_Effort %>%
    select(UTC, Status)

  noise <- NULL

  list(
    buoys = transpose(list(position = position, calibration = calibration)),
    detections = detections,
    effort = effort,
    noise = noise
  )
}
