#' Morph raw map data into a road network
#'
#' `morph_road_network()` converts map data from the RoboCup Rescue Simulation
#' into a road network.
#'
#' This road network consists of nodes and edges. Nodes are defined as
#' the centroids of all areas and the center points the boundaries
#' between areas. Edges connect nodes between an agent can travel.
#'
#' @name morph_road_network
#' @param map_data `rrs_map` data, the GML map data loaded by
#'   the `read_rrs_map()`.
#' @return A list containing the network components:
#'   - `nodes`: A sf object of nodes with point geometries.
#'   - `edges`: A sf object of edges with linestring geometries.
#' @examples
#' gml <- system.file("extdata", "map-test.gml", package = "rrstools")
#' map_data <- read_rrs_map(gml)
#' network <- morph_road_network(map_data)
#' network
NULL

# Extract nodes from map data
extract_network_nodes <- function(map_data) {
  # Get face attributes data
  face_attributes <- map_data$raw_parsed_data$faces

  # Create face nodes from building or road
  faces_by_id <- split(face_attributes, face_attributes$id)
  face_nodes <- do.call(rbind, lapply(faces_by_id, function(face) {
    data.frame(
      id     = face$id[1],
      type   = face$type[1],
      degree = sum(!is.na(face$neighbour))
    )
  }))

  # Creates border nodes from edges with neighbours
  has_neighbour <- !is.na(face_attributes$neighbour)
  boarder_ids <- unique(face_attributes$edge_hrefs[has_neighbour])
  boarder_nodes <- data.frame(
    id     = boarder_ids,
    type   = "border",
    degree = 2
  )

  rbind(face_nodes, boarder_nodes)
}

# Extract edges from map data
extract_network_edges <- function(map_data) {
  # Get face attributes data
  face_attributes <- map_data$raw_parsed_data$faces

  # Create a list of faces
  faces_by_id <- split(face_attributes, face_attributes$id)

  # Create edges from face data connecting the face node to its border nodes
  edge_list <- lapply(faces_by_id, function(face) {
    data.frame(
      id   = face$id[1],
      type = face$type[1],
      from = face$id[1],
      to   = unique(face$edge_hrefs[!is.na(face$neighbour)])
    )
  })
  do.call(rbind, unname(edge_list))
}

# Create network node simple features
create_network_node_sf <- function(nodes, map_data) {
  # Extract sf data for border nodes
  border_node_ids <- nodes$id[nodes$type == "border"]
  border_sf <- map_data$edge_sf[match(border_node_ids, map_data$edge_sf$id), ]

  # Combine all features geometries
  all_feature_sf <- rbind(
    map_data$building_sf,
    map_data$road_sf,
    border_sf
  )

  # Calculate centroids for all features
  feature_centroid_sf <- all_feature_sf |>
    sf::st_set_agr("constant") |>
    sf::st_centroid()

  # Match node attributes with their corresponding centroids
  ordered_geometries <- feature_centroid_sf$geometry[match(nodes$id, feature_centroid_sf$id)]

  sf::st_sf(nodes, geometry = ordered_geometries)
}

# Create network edge simple features
create_network_edge_sf <- function(edges, node_sf) {
  # Get the start and end point geometries
  start_node <- node_sf$geometry[match(edges$from, node_sf$id), ]
  end_node   <- node_sf$geometry[match(edges$to, node_sf$id), ]

  # Create a list of linestring geometries connecting the start and end points
  geometries <- mapply(
    function(p1, p2) sf::st_linestring(c(p1, p2)),
    start_node, end_node, SIMPLIFY = FALSE
  )

  sf::st_sf(edges, geometry = geometries)
}

#' @rdname morph_road_network
#' @export
morph_road_network <- function(map_data) {
  # Extract node and edge attribute tables from raw data
  nodes <- extract_network_nodes(map_data)
  edges <- extract_network_edges(map_data)

  # Create sf objects
  node_sf <- create_network_node_sf(nodes, map_data)
  edge_sf <- create_network_edge_sf(edges, node_sf)

  road_network <- list(
    nodes = node_sf,
    edges = edge_sf
  )
  class(road_network) <- c("rrs_road_network", "list")

  return(road_network)
}
