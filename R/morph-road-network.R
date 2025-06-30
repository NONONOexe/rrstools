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
#' @param map `rrs_map` data, the GML map data loaded by
#'   the `read_rrs_map()`.
#' @return A list containing the network components:
#'   - `nodes`: A sf object of nodes with point geometries.
#'   - `edges`: A sf object of edges with linestring geometries.
#' @examples
#' gml <- system.file("extdata", "map-test.gml", package = "rrstools")
#' map <- read_rrs_map(gml)
#' net <- morph_road_network(map)
#' net
NULL

# Create network nodes from map data
create_network_nodes <- function(map) {
  faces <- get_faces(map)

  # Create face nodes
  last_id <- max(as.integer(c(map$nodes$id, map$edges$id, map$faces$id)))
  face_nodes <- sf::st_sf(
    id        = as.character(last_id + seq(nrow(faces))),
    origin_id = faces$id,
    type      = faces$type,
    degree    = sapply(faces$edges,
                      function(face_edges) sum(!is.na(face_edges$neighbour))),
    geometry  = sf::st_centroid(faces$geometry)
  )

  # Creates border nodes
  all_face_edges <- do.call(rbind, faces$edges)
  has_neighbour <- !is.na(all_face_edges$neighbour)
  border_ids <- unique(all_face_edges$edge_hrefs[has_neighbour])
  borders <- get_edges(map, ids = border_ids)
  last_id <- max(as.integer(face_nodes$id))
  border_nodes <- sf::st_sf(
    id        = as.character(last_id + seq(nrow(borders))),
    origin_id = borders$id,
    type      = "border",
    degree    = 2,  # Each border node connects to two faces
    geometry  = sf::st_centroid(borders$geometry)
  )

  rbind(face_nodes, border_nodes)
}

# Create network edges from map data
create_network_edges <- function(map, network_nodes) {
  # Create edges from the face node to its border nodes
  faces <- get_faces(map)
  neighbour_num <- faces$edges |>
    sapply(function(edges) sum(!is.na(edges$neighbour)))
  last_id <- max(as.integer(network_nodes$id))
  network_edges <- data.frame(
    id   = as.character(last_id + seq(sum(neighbour_num))),
    type = rep(faces$type, times = neighbour_num),
    from = rep(faces$id, times = neighbour_num),
    to   = faces$edges |>
      sapply(function(face_edges)
        face_edges$edge_hrefs[!is.na(face_edges$neighbour)]) |>
      unlist(use.names = FALSE)
  )

  # Get the start and end point geometries
  origin_ids <- network_nodes$origin_id
  start_node <- network_nodes[match(network_edges$from, origin_ids), ]$geometry
  end_node   <- network_nodes[match(network_edges$to, origin_ids), ]$geometry

  # Create a list of linestring geometries connecting the start and end points
  geometries <- mapply(
    function(p1, p2) sf::st_linestring(c(p1, p2)),
    start_node, end_node, SIMPLIFY = FALSE
  )

  sf::st_sf(network_edges, geometry = geometries)
}

#' @rdname morph_road_network
#' @export
morph_road_network <- function(map) {
  network_nodes <- create_network_nodes(map)
  network_edges <- create_network_edges(map, network_nodes)
  road_network  <- list(
    nodes = network_nodes,
    edges = network_edges
  )
  class(road_network) <- c("rrs_road_network", "list")

  return(road_network)
}
