#' Get elements from a RoboCupRescue Simulation map
#'
#' These functions provide methods to extract specific geographical elements
#' from an `rrs_map` object. You can retrieve all elements of a certain type
#' or filter them by their IDs.
#'
#' @name get_elements
#' @param map An object of class `rrs_map`.
#' @param ids A character vector of element IDs to filter by.
#'   If empty (default), all elements of the specified type are returned.
#' @return An `sf` data frame containing the requested map elements.
#' @examples
#' gml <- system.file("extdata", "map-test.gml", package = "rrstools")
#' map <- read_rrs_map(gml)
#'
#' # Get all nodes
#' get_nodes(map)
#'
#' # Get a specific node by ID
#' get_nodes(map, c("0", "2"))
NULL

# Retrieve elements by type and IDs
get_elements <- function(map,
                         ids  = character(),
                         type = c("node", "edge", "face")) {
  type <- match.arg(type)
  if (type == "node") elements <- map$nodes
  if (type == "edge") elements <- map$edges
  if (type == "face") elements <- map$faces

  if (length(ids) != 0) {
    elements[elements$id %in% ids, ]
  } else {
    elements
  }
}

#' @rdname get_elements
#' @export
get_nodes <- function(map, ids = character()) {
  get_elements(map, ids, type = "node")
}

#' @rdname get_elements
#' @export
get_edges <- function(map, ids = character()) {
  get_elements(map, ids, type = "edge")
}

# Retrieve faces (buildings or roads) by IDs and type
get_faces <- function(map, ids = character(), type = NULL) {
  faces <- get_elements(map, ids, type = "face")
  if (!is.null(type)) {
    faces[faces$type == type, ]
  } else {
    faces
  }
}

#' @rdname get_elements
#' @export
get_roads <- function(map, ids = character()) {
  get_faces(map, ids, type = "road")
}

#' @rdname get_elements
#' @export
get_buildings <- function(map, ids = character()) {
  get_faces(map, ids, type = "building")
}

# Check if a set of edges forms a single linestring
is_single_linestring <- function(edges) {
  node_ids <- unique(c(edges$start_node_id, edges$end_node_id))
  nrow(edges) + 1 == length(node_ids)
}

# Determine if a road is an entrance of a building
is_entrance <- function(road_edges, map) {
  is_road_border <- road_edges$neighbour %in% get_roads(map)$id
  if (all(!is_road_border)) return(FALSE)

  is_building_border <- road_edges$neighbour %in% get_buildings(map)$id
  if (all(!is_building_border)) return(FALSE)

  road_border_ids <- road_edges[is_road_border, ]$edge_hrefs
  road_borders <- get_edges(map, ids = road_border_ids)
  if (!is_single_linestring(road_borders)) return(FALSE)

  building_border_ids <- road_edges[is_building_border, ]$edge_hrefs
  building_borders <- get_edges(map, ids = building_border_ids)
  if (!is_single_linestring(building_borders)) return(FALSE)

  return(TRUE)
}

#' @rdname get_elements
#' @export
get_entrances <- function(map, ids = character()) {
  roads <- get_roads(map, ids)
  is_entrance_vec <- sapply(roads$edges, is_entrance, map)
  entrance_ids <- names(is_entrance_vec)[is_entrance_vec]
  roads[roads$id %in% entrance_ids, ]
}
