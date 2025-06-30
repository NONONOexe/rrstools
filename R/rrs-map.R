#' Print a rrs map object
#'
#' `print` prints a concise summary of a `rrs_map` object.
#' It displays the object type, the number of nodes, edges, and faces it
#' contains, its overall bounding box.
#'
#' @param x An object class `rrs_map`.
#' @param ... Not used.
#' @examples
#' gml <- system.file("extdata", "map-test.gml", package = "rrstools")
#' map <- read_rrs_map(gml)
#' map
#' @export
print.rrs_map <- function(x, ...) {
  cat("RoboCupRescue Simulation map object\n")

  cat(sprintf("Number of elements: nodes: %d edges: %d faces: %d\n",
      nrow(x$nodes), nrow(x$edges), nrow(x$faces)))

  bbox <- sf::st_bbox(x$nodes)
  cat(sprintf("Bounding box: xmin: %d ymin: %d xmax: %d ymax: %d\n",
      bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]))

  invisible(x)
}

#' Plot a rrs map
#'
#' `plot.rrs_map` visualizes a `rrs_map` object, displaying the geographical
#' features such as buildings and roads. It leverages the `sf` package's
#' plotting capbilities to render the map components.
#'
#' @param x An object class `rrs_map`.
#' @param building_colour The colour of the buildings.
#' @param building_border The border colour of the buildings.
#' @param road_colour The colour of the roads.
#' @param road_border The border colour of the roads.
#' @param background_colour The background colour of the plot.
#' @param ... Additional arguments passed to `par`.
#' @examples
#' gml <- system.file("extdata", "map-test.gml", package = "rrstools")
#' map <- read_rrs_map(gml)
#' plot(map)
#' @export
plot.rrs_map <- function(x,
                         building_colour   = "#f0e7d8",
                         building_border   = "#121212",
                         road_colour       = "#dbdbdb",
                         road_border       = "#121212",
                         background_colour = NA,
                         ...) {
  graphics::par(...)
  plot(
    get_buildings(x)$geometry,
    col    = building_colour,
    border = building_border,
    bg     = background_colour
  )
  plot(
    get_roads(x)$geometry,
    col    = road_colour,
    border = road_border,
    add    = TRUE
  )

  return(invisible(NULL))
}
