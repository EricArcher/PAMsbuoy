#' @title Effort Duration
#' @description Calculate duration of effort segments.
#'
#' @param df data.frame of effort data
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
effortDuration <- function(df) {
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
