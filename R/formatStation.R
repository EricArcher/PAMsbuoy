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
  position <- db$HydrophoneStreamers %>%
    mutate(Buoy = as.character(StreamerIndex)) %>%
    select(Buoy, UTC, Latitude, Longitude) %>%
    arrange(Buoy, UTC) %>%
    split(., .$Buoy)

  calibration <- db$DIFAR_Localisation %>%
    mutate(
      Species = tolower(str_trim(Species)),
      Buoy = as.character(Channel)
    ) %>%
    filter(Species == "vessel") %>%
    select(Buoy, UTC, DIFARBearing) %>%
    arrange(Buoy, UTC) %>%
    split(., .$Buoy) %>%
    calculateOffset(position, db)

  transpose(list(position = position, calibration = calibration))
}