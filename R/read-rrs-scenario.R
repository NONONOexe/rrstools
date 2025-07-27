#' Read RoboCupRescue Simulation scenario data from XML file
#'
#' `read_rrs_scenario()` reads and processes scenario data for
#' RoboCupRescue Simulation from a XML file.
#'
#' @param xml Path to the XML file containing the scenario data.
#' @return A `rrs_scenario` object. This object is a `data.frame`, and contains
#'   the following columns:
#'   \itemize{
#'     \item `type`: Character. The type of the entity
#'       (e.g., "refuge", "civilian").
#'     \item `location`: Character. The entity ID of the location where
#'       the entity is placed.
#'     \item `attributes`: List. A list containing other attributes of the
#'       entity.
#'   }
#' @export
#' @examples
#' xml <- system.file("extdata", "scenario-test.xml", package = "rrstools")
#' scenario <- read_rrs_scenario(xml)
#' scenario
read_rrs_scenario <- function(xml) {
  # Load the XML structure
  xml <- xml2::read_xml(xml)

  # Extract entities from the XML
  entities <- xml2::xml_find_all(
    xml,
    paste(paste0(
      ".//scenario:",
      c("refuge", "civilian", "firebrigade", "firestation", "policeforce",
        "policeoffice", "ambulanceteam", "ambulancecentre")
    ), collapse = "|")
  )

  # Extract attributes and create a data frame
  rrs_scenario <- data.frame(
    type       = xml2::xml_name(entities),
    location   = xml2::xml_attr(entities, "location"),
    attributes = I(lapply(
      entities,
      function(entity) {
        list(
          bed_capacity = as.integer(xml2::xml_attr(entity, "bedCapacity"))
        )
      }
    ))
  )
  class(rrs_scenario) <- c("rrs_scenario", "data.frame")
}
