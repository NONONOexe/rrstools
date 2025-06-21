#' Read RoboCupRescue Simulation map data from GML file
#'
#' This function reads and processes map data for RoboCupRescue Simulation
#' from a GML file. It extracts nodes, edges, buildings and roads, and
#' organizes them into a list.
#'
#' @name read_rrs_map
#' @param gml Path to the GML file.
#' @param scale_data Logical. If `TRUE`, coordinates are scaled up by a factor
#'   of 1000 to match the simulation environment, and adjusted such that
#'   the minimum x and y values are 0. (Default: `TRUE`)
#' @return A list of `sf` objects: nodes, edges, buildings, and roads.
#' @examples
#' gml <- system.file("extdata", "map-test.gml", package = "rrstools")
#' map_data <- read_rrs_map(gml)
#' map_data
NULL

# Parse nodes from the XML and return a sf object
parse_nodes <- function(xml, scale_data) {
  nodes <- xml2::xml_find_all(xml, ".//gml:Node")
  coordinates <- strsplit(xml2::xml_text(nodes), ",")

  x <- as.numeric(sapply(coordinates, "[[", 1))
  y <- as.numeric(sapply(coordinates, "[[", 2))
  if (scale_data) {
    x <- x * 1000
    y <- y * 1000
    x <- x - min(x)
    y <- y - min(y)
  }

  return(sf::st_as_sf(
    data.frame(id = xml2::xml_attr(nodes, "id"), x, y),
    coords = c("x", "y"), crs = NA, agr = "constant"
  ))
}

# Extract node references and orientations from edges
extract_edge_nodes <- function(edges) {
  directed_nodes <- xml2::xml_find_all(edges, ".//gml:directedNode")
  node_ref <- sub("^#", "", xml2::xml_attr(directed_nodes, "href"))
  orientations <- xml2::xml_attr(directed_nodes, "orientation")

  return(list(
    source = node_ref[orientations == "+"],
    target = node_ref[orientations == "-"]
  ))
}

# Parse edges and create a sf object
parse_edges <- function(xml, node_sf) {
  edges <- xml2::xml_find_all(xml, ".//gml:Edge")
  edge_nodes <- extract_edge_nodes(edges)

  source_nodes <- node_sf$geometry[match(edge_nodes$source, node_sf$id)]
  target_nodes <- node_sf$geometry[match(edge_nodes$target, node_sf$id)]

  geometries <- mapply(
    function(p1, p2) sf::st_linestring(c(p1, p2)),
    source_nodes, target_nodes, SIMPLIFY = FALSE
  )

  return(sf::st_sf(
    id = xml2::xml_attr(edges, "id"),
    geometry = geometries, crs = NA, agr = "constant"
  ))
}

# Group and combine edges for buildings or roads
group_edges_by_face <- function(edges, faces) {
  direct_edges <- xml2::xml_find_all(faces, ".//gml:directedEdge")
  edge_ids <- sub("^#", "", xml2::xml_attr(direct_edges, "href"))
  matched_edges <- edges$geometry[match(edge_ids, edges$id)]
  group_indices <- rep(seq_along(faces), xml2::xml_length(faces))

  return(split(matched_edges, group_indices))
}

# Parse faces (building or roads) and create a sf object
parse_faces <- function(xml, edge_sf, type = c("building", "road")) {
  type <- match.arg(type)
  features <- xml2::xml_find_all(xml, paste0(".//rcr:", type))
  faces <- xml2::xml_find_all(features, ".//gml:Face")
  edge_groups <- group_edges_by_face(edge_sf, faces)

  geometries <- sapply(edge_groups, function(lines) {
    combined_lines <- sf::st_union(lines)
    sf::st_collection_extract(sf::st_polygonize(combined_lines), "POLYGON")
  })

  return(sf::st_sf(
    id = xml2::xml_attr(features, "id"),
    geometry = geometries, crs = NA, agr = "constant"
  ))
}

#' @rdname read_rrs_map
#' @export
read_rrs_map <- function(gml, scale_data = FALSE) {
  xml <- xml2::read_xml(gml)

  node_sf <- parse_nodes(xml, scale_data)
  edge_sf <- parse_edges(xml, node_sf)
  building_sf <- parse_faces(xml, edge_sf, "building")
  road_sf <- parse_faces(xml, edge_sf, "road")

  rrs_map <- list(
    nodes = node_sf,
    edges = edge_sf,
    buildings = building_sf,
    roads = road_sf
  )
  class(rrs_map) <- c("rrs_map", "list")

  return(rrs_map)
}
