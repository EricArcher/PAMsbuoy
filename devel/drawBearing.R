#' Add Bearings to a Plot
#'
#' Add bearing lines as arrows to an existing ggplot object
#'
#' @param df a data frame containing the coordinates \code{Latitude} and \code{Longitude}
#'   of the point to draw the bearing from.
#' @param plot the ggplot object to add the bearing lines to.
#' @param bearing the name of the column containing the bearing to draw
#' @param distance length (in km) of the bearing lines.
#' @param \dots arguments passed to the geom (ie. alpha, color).
#'
#' @return A ggplot object with bearing lines drawn as arrows from the given coordinates.
#'
#' @author Taiki Sakai \email{taiki.sakai@noaa.gov}
#'
#' @examples
#' require(ggplot2)
#' df <- data.frame(Latitude=32.64, Longitude=-117.41, DIFARBearing=75)
#' g <- ggplot(data=df, aes(x=Longitude, y=Latitude)) + geom_point()
#' drawBearings(df, g)
#'
#' @importFrom swfscMisc destination
#' @importFrom ggplot2 geom_segment
#' @export
#'
drawBearings <- function(df, map=NULL, bearing = 'DIFARBearing', distance=1, ...) {
  data <- df  # Doing this because geom_ was getting angry using data=df, may try to fix later
  endcoord <- t(mapply(swfscMisc::destination, data$Latitude, data$Longitude,
                       data[[bearing]], distance, units='km'))
  data$endlat <- endcoord[,1]
  data$endlong <- endcoord[,2]
  if(is.null(map)) {
    map <- getMap(positions=data.frame(Longitude = c(data$Longitude, data$endlong),
                                       Latitude = c(data$Latitude, data$endlat)),
                  center = c(lon = mean(unique(data$Longitude)),
                             lat = mean(unique(data$Latitude))),
                  zoom=18)
  }
  map <- ggplot() + xlim(min(c(data$Longitude, data$endlong)), max(c(data$Longitude, data$endlong))) +
    ylim(min(c(data$Latitude, data$endlat)), max(c(data$Latitude, data$endlat)))
  manipulate({
    map + geom_point(data=data, aes_string(x='Longitude', y='Latitude', color='Buoy'), size=3) +
      geom_segment(data=data[data$detection %in% detPicker,], aes_string(x='Longitude', y='Latitude',
                                                                         xend='endlong', yend='endlat', ...),
                   arrow=arrow(length=unit(.3, 'cm'), type='closed'))},
    detPicker = picker(as.list(unique(data$detection))))

}
