#' @title Format Effort
#' @description Format effort for each buoy by removing noise and creating
#'   blocks of alternating effort
#'
#' @param db list of data frames from PAMGuard SQLlite database
#'
#' @return a list of effort windows for each buoy
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @importFrom dplyr mutate select arrange filter rename
#' @importFrom tidyr spread
#'
formatEffort <- function(db) {
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


