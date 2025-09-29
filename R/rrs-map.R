#' Print a RoboCupRescue Simulation map object
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
  cat(sprintf("Bounding box: xmin: %f ymin: %f xmax: %f ymax: %f\n",
      bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]))

  invisible(x)
}

#' Plot a RoboCupRescue Simulation map
#'
#' `plot` visualizes a `rrs_map` object, displaying the geographical
#' features such as buildings and roads. If a `rrs_scenario` object is
#' provided, it also overlays the initial locations of rescue platoons,
#' civilians, and special buildings from the scenario.
#' It leverages the `sf` package's plotting capabilities to render
#' the map components.
#'
#' @details
#' `plot` first plots the basic map layout, including buildings and roads.
#' Then, if a scenario is given, it plots the following entities:
#' \itemize{
#'   \item \strong{Refuges}: Plotted in green (`#00ff00`), with the capacity
#'         displayed.
#'   \item \strong{Fire Stations}: Plotted in yellow (`#ffff00`).
#'   \item \strong{Ambulance Centres}: Plotted in white (`#ffffff`).
#'   \item \strong{Police Offices}: Plotted in blue (`#0000ff`).
#'   \item \strong{Civilians}: Plotted as green circles (`#00ff00`),
#'         with the number indicating the count at that location.
#'   \item \strong{Fire Brigades}: Plotted as red circles (`#ff0000`),
#'         with the number indicating the count at that location.
#'   \item \strong{Ambulance Teams}: Plotted as white circles (`#ffffff`),
#'         with the number indicating the count at that location.
#'   \item \strong{Police Forces}: Plotted as blue circles (`#0000ff`),
#'         with the number indicating the count at that location.
#' }
#' The positions of human entities (agents and civilians) are slightly offset
#' from the center of their location to avoid overlap.
#'
#' @param x An object class `rrs_map`.
#' @param scenario An object of class `rrs_scenario`. If provided, entities
#'   from the scenario will be plotted on the map. Defaults to `NULL`.
#' @param building_colour The colour of the buildings.
#' @param building_border The border colour of the buildings.
#' @param road_colour The colour of the roads.
#' @param road_border The border colour of the roads.
#' @param background_colour The background colour of the plot.
#' @param ... Additional arguments passed to `plot()`.
#'
#' @examples
#' gml <- system.file("extdata", "map-test.gml", package = "rrstools")
#' map <- read_rrs_map(gml)
#'
#' # Plot only the map
#' plot(map)
#'
#' # Plot the map with a scenario
#' xml <- system.file("extdata", "scenario-test.xml", package = "rrstools")
#' scenario <- read_rrs_scenario(xml)
#' plot(map, scenario)
#'
#' @export
plot.rrs_map <- function(x,
                         scenario = NULL,
                         building_colour   = "#f0e7d8",
                         building_border   = "#121212",
                         road_colour       = "#dbdbdb",
                         road_border       = "#121212",
                         background_colour = NA,
                         ...) {
  plot(
    get_buildings(x)$geometry,
    col    = building_colour,
    border = building_border,
    bg     = background_colour,
    ...
  )
  plot(
    get_roads(x)$geometry,
    col    = road_colour,
    border = road_border,
    add    = TRUE
  )

  if (!is.null(scenario)) {
    if (!inherits(scenario, "rrs_scenario")) {
      stop("`scenario` must be of class `rrs_scenario`.")
    }

    plot_refuges(x$faces, scenario, building_border)
    plot_fire_station(x$faces, scenario, building_border)
    plot_ambulance_centre(x$faces, scenario, building_border)
    plot_police_office(x$faces, scenario, building_border)

    plot_civilians(x$faces, scenario)
    plot_fire_brigades(x$faces, scenario)
    plot_ambulance_teams(x$faces, scenario)
    plot_police_forces(x$faces, scenario)
  }

  return(invisible(NULL))
}

plot_refuges <- function(faces, scenario, building_border) {
  plot_buildings(faces, scenario, "refuge",
                 fill_colour   = "#00ff00",
                 border_colour = building_border)
}

plot_fire_station <- function(faces, scenario, building_border) {
  plot_buildings(faces, scenario, "firestation",
                 fill_colour   = "#ffff00",
                 border_colour = building_border)
}

plot_ambulance_centre <- function(faces, scenario, building_border) {
  plot_buildings(faces, scenario, "ambulancecentre",
                 fill_colour   = "#ffffff",
                 border_colour = building_border)
}

plot_police_office <- function(faces, scenario, building_border) {
  plot_buildings(faces, scenario, "policeoffice",
                 fill_colour   = "#0000ff",
                 border_colour = building_border)
}

plot_civilians <- function(faces, scenario) {
  plot_humans(faces, scenario, "civilian",
              fill_colour = "#00ff00",
              x_offset    = 2.0,
              y_offset    = 0.0)
}

plot_fire_brigades <- function(faces, scenario) {
  plot_humans(faces, scenario, "firebrigade",
              fill_colour = "#ff0000",
              x_offset    = 0.0,
              y_offset    = 2.0)
}

plot_ambulance_teams <- function(faces, scenario) {
  plot_humans(faces, scenario, "ambulanceteam",
              fill_colour = "#ffffff",
              x_offset    = -2.0,
              y_offset    = 0.0)
}

plot_police_forces <- function(faces, scenario) {
  plot_humans(faces, scenario, "policeforce",
              fill_colour = "#0000ff",
              text_colour = "#ffffff",
              x_offset    = 0.0,
              y_offset    = -2.0)
}

plot_buildings <- function(faces,
                           scenario,
                           building_type,
                           fill_colour,
                           text_colour   = "#000000",
                           border_colour = "#000000") {
  buildings <- scenario[scenario$type == building_type, ]

  if (nrow(buildings) == 0) {
    return(invisible(NULL))
  }

  plot_data <- merge(faces, buildings, by.x = "id", by.y = "location")

  if (nrow(plot_data) == 0) {
    return(invisible(NULL))
  }

  plot(
    plot_data$geometry,
    col    = fill_colour,
    border = border_colour,
    add    = TRUE
  )

  if (building_type != "refuge") {
    return(invisible(NULL))
  }

  sf::st_agr(plot_data) <- "constant"
  centroids <- sf::st_centroid(plot_data)

  graphics::text(
    x      = sf::st_coordinates(centroids)[, 1],
    y      = sf::st_coordinates(centroids)[, 2],
    labels = paste0("C = ", unlist(centroids$attributes)),
    col    = text_colour,
    cex    = 0.9,
    font   = 2
  )

  return(invisible(NULL))
}

plot_humans <- function(faces,
                        scenario,
                        human_type,
                        fill_colour,
                        text_colour = "#000000",
                        x_offset    = 0,
                        y_offset    = 0) {
  humans <- scenario[scenario$type == human_type, ]

  if (nrow(humans) == 0) {
    return(invisible(NULL))
  }

  humans$n <- 1
  civilians_n <- stats::aggregate(n ~ location, data = humans, FUN = sum)
  plot_data <- merge(faces, civilians_n, by.x = "id", by.y = "location")

  if (nrow(plot_data) == 0) {
    return(invisible(NULL))
  }

  sf::st_agr(plot_data) <- "constant"
  centroids <- sf::st_centroid(plot_data)

  plot(
    centroids$geometry + c(x_offset, y_offset),
    pch = 21,
    bg  = fill_colour,
    cex = 2.0,
    lwd = 1.0,
    add = TRUE
  )

  graphics::text(
    x      = sf::st_coordinates(centroids)[, 1] + x_offset,
    y      = sf::st_coordinates(centroids)[, 2] + y_offset,
    labels = centroids$n,
    col    = text_colour,
    cex    = 0.9,
    font   = 2
  )

  return(invisible(NULL))
}
