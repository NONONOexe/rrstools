#' Plot a rrs map
#'
#' This function plots a rrs map
#' @param x An object class `rrs_map`.
#' @param building_colour The colour of the buildings.
#' @param building_border The border colour of the buildings.
#' @param road_colour The colour of the roads.
#' @param road_border The border colour of the roads.
#' @param background_colour The background colour of the plot.
#' @param ... Additional arguments passed to `par`.
#' @examples
#' gml <- system.file("extdata", "map-test.gml", package = "rrstools")
#' map_data <- read_rrs_map(gml)
#' plot(map_data)
#' @export
plot.rrs_map <- function(x,
                         building_colour   = "#f0e7d8",
                         building_border   = "#121212",
                         road_colour       = "#dbdbdb",
                         road_border       = "#121212",
                         background_colour = NA,
                         ...) {
  par(...)
  plot(
    x$buildings$geometry,
    col    = building_colour,
    border = building_border,
    bg     = background_colour
  )
  plot(
    x$roads$geometry,
    col    = road_colour,
    border = road_border,
    add    = TRUE
  )

  return(invisible(NULL))
}
