globalVariables("competition_maps")

#' RoboCupRescue Simulation competition maps
#'
#' A dataset containing information about the maps used in past RoboCupRescue
#' Simulation competitions. It includes the competitions year, the name of
#' the map, and the official URL to download the map data.
#'
#' @format A `data.frame` with 15 rows and 3 columns:
#' \describe{
#'   \item{year}{An integer representing the year of the competition.}
#'   \item{map}{A character string indicating the name of the map data.}
#'   \item{url}{A character string containing the official download URL for the
#'              map data.}
#' }
#' @examples
#' # Show the dataset
#' competition_maps
#'
#' @name competition_maps
"competition_maps"
