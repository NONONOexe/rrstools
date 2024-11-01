#' Read RoboCupRescue Simulation map data from GML file
#'
#' This function reads and processes map data for RoboCupRescue Simulation
#' from a GML file. It extracts nodes, edges, buildings and roads, and
#' organizes them into a list.
#'
#' @name read_rrs_map
#' @param gml Path to the GML file.
#' @return A list of `sf` objects: nodes, edges, buildings, and roads.
#' @examples
#' gml <- system.file("extdata", "map.gml", package = "rrstools")
#' map_data <- read_rrs_map(gml)
#' map_data
NULL

# Parse nodes from the XML and return a sf object
parse_nodes <- function(xml) {
  nodes <- xml_find_all(xml, ".//gml:Node")
  coordinates <- strsplit(xml_text(nodes), ",")

  x <- as.numeric(sapply(coordinates, "[[", 1))
  y <- as.numeric(sapply(coordinates, "[[", 2))

  return(st_as_sf(
    data.frame(id = xml_attr(nodes, "id"), x, y),
    coords = c("x", "y"), crs = NA, agr = "constant"
  ))
}

# Extract node references and orientations from edges
extract_edge_nodes <- function(edges) {
  directed_nodes <- xml_find_all(edges, ".//gml:directedNode")
  node_ref <- sub("^#", "", xml_attr(directed_nodes, "href"))
  orientations <- xml_attr(directed_nodes, "orientation")

  return(list(
    source = node_ref[orientations == "+"],
    target = node_ref[orientations == "-"]
  ))
}

# Parse edges and create a sf object
parse_edges <- function(xml, node_sf) {
  edges <- xml_find_all(xml, ".//gml:Edge")
  edge_nodes <- extract_edge_nodes(edges)

  source_nodes <- node_sf$geometry[match(edge_nodes$source, node_sf$id)]
  target_nodes <- node_sf$geometry[match(edge_nodes$target, node_sf$id)]

  geometries <- mapply(
    function(p1, p2) st_linestring(c(p1, p2)),
    source_nodes, target_nodes, SIMPLIFY = FALSE
  )

  return(st_sf(
    id = xml_attr(edges, "id"),
    geometry = geometries, crs = NA, agr = "constant"
  ))
}

# Group and combine edges for buildings or roads
group_edges_by_face <- function(edges, faces) {
  direct_edges <- xml_find_all(faces, ".//gml:directedEdge")
  edge_ids <- sub("^#", "", xml_attr(direct_edges, "href"))
  matched_edges <- edges$geometry[match(edge_ids, edges$id)]
  group_indices <- rep(seq_along(faces), xml_length(faces))

  return(split(matched_edges, group_indices))
}

# Parse faces (building or roads) and create a sf object
parse_faces <- function(xml, edge_sf, type = c("building", "road")) {
  type <- match.arg(type)
  features <- xml_find_all(xml, paste0(".//rcr:", type))
  faces <- xml_find_all(features, ".//gml:Face")
  edge_groups <- group_edges_by_face(edge_sf, faces)

  geometries <- sapply(edge_groups, function(lines) {
    combined_lines <- st_union(lines)
    st_collection_extract(st_polygonize(combined_lines), "POLYGON")
  })

  return(st_sf(
    id = xml_attr(features, "id"),
    geometry = geometries, crs = NA, agr = "constant"
  ))
}

# Read a GML map and extract nodes, edges, buildings, and roads
#' @rdname read_rrs_map
#' @export
read_rrs_map <- function(gml) {
  xml <- read_xml(gml)

  node_sf <- parse_nodes(xml)
  edge_sf <- parse_edges(xml, node_sf)
  building_sf <- parse_faces(xml, edge_sf, "building")
  road_sf <- parse_faces(xml, edge_sf, "road")

  return(list(
    nodes = node_sf,
    edges = edge_sf,
    buildings = building_sf,
    roads = road_sf
  ))
}
