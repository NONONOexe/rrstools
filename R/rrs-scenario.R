#' Print a RRS scenario object
#'
#' `print` prints a concise summary of a `rrs_scenario` object.
#' It displays the object type, the total number of entities, and a
#' table with the counts of entities for each type.
#'
#' @param x An object class `rrs_scenario`.
#' @param ... Not used.
#' @examples
#' xml <- system.file("extdata", "scenario-test.xml", package = "rrstools")
#' scenario <- read_rrs_scenario(xml)
#' scenario
#' @export
print.rrs_scenario <- function(x, ...) {
  cat("RoboCupRescue Simulation scenario object\n")

  num_entities <- nrow(x)
  cat(sprintf("Number of entities: %d\n", num_entities))

  cat("Entities by type:\n")
  entity_counts <- as.data.frame(table(x$type))
  colnames(entity_counts) <- c("Type", "Count")
  print(entity_counts, row.names = FALSE)

  invisible(x)
}

#' Plot a RRS scenario object (Not supported directly)
#'
#' A `rrs_scenario` object cannot be plotted by itself because it lacks the
#' necessary spatial conext provided by a map. This function exists to
#' intercept calls to `plot(scenario)` and provide a helpful error message.
#'
#' To visualize a scenario, you must plot it together with a `rrs_map`
#' object.
#'
#' @param x An object class `rrs_scenario`.
#' @param ... Not used.
#' @usage
#' # Collect usage:
#' # plot(map, scenario)
plot.rrs_scenario <- function(x, ...) {
  stop(
    "Cannot plot a scenario without map data.\n",
    "Please use `plot(map, scenario)` to visualize the scenario.",
    call. = FALSE
  )
}
