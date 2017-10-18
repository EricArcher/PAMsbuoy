#' Add Bearings to a Plot
#'
#' Add bearing lines as arrows to an existing ggplot object
#'
#' @param df a data frame containing the coordinates \code{Latitude} and \code{Longitude}
#'   of the point to draw the bearing \code{DIFARBearing} from.
#' @param plot the ggplot object to add the bearing lines to.
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
drawBearings <- function(df, plot, distance=1, ...) {
    data <- df  # Doing this because geom_ was getting angry using data=df, may try to fix later
    endcoord <- t(mapply(destination, data$Latitude, data$Longitude, data$DIFARBearing, distance, units='km'))
    data$endlat <- endcoord[,1]
    data$endlong <- endcoord[,2]
    plot + geom_segment(data=data, aes(x=Longitude, y=Latitude, xend=endlong, yend=endlat),
                        arrow=arrow(type='closed', angle=15), ...)
}
