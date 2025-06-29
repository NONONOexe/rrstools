#' Read RoboCupRescue Simulation map data from GML file
#'
#' This function reads and processes map data for RoboCupRescue Simulation
#' from a GML file. It extracts nodes, edges, buildings and roads.
#'
#' @name read_rrs_map
#' @param gml Path to the GML file.
#' @param scale_data Logical. If `TRUE`, coordinates are scaled up by a factor
#'   of 1000 to match the simulation environment, and adjusted such that
#'   the minimum x and y values are 0. (Default: `TRUE`)
#' @return A list of `sf` objects: nodes, edges, buildings, and roads.
#' @examples
#' gml <- system.file("extdata", "map-test.gml", package = "rrstools")
#' map <- read_rrs_map(gml)
#' map
NULL

# Extract href attribute from an feature element
extract_href_value <- function(feature) {
  sub("#", "", xml2::xml_attr(feature, "href"))
}

# Parse node data from XML content
parse_nodes <- function(xml) {
  nodes  <- xml2::xml_find_all(xml, ".//gml:Node")
  coords <- strsplit(xml2::xml_text(nodes), ",")

  data.frame(
    id = xml2::xml_attr(nodes, "id"),
    x  = as.numeric(sapply(coords, "[[", 1)),
    y  = as.numeric(sapply(coords, "[[", 2))
  )
}

# Parse edge data from XML content
parse_edges <- function(xml) {
  edges          <- xml2::xml_find_all(xml, ".//gml:Edge")
  directed_nodes <- xml2::xml_find_all(edges, ".//gml:directedNode")
  node_hrefs     <- extract_href_value(directed_nodes)
  orientations   <- xml2::xml_attr(directed_nodes, "orientation")

  data.frame(
    id            = xml2::xml_attr(edges, "id"),
    start_node_id = node_hrefs[orientations == "-"],
    end_node_id   = node_hrefs[orientations == "+"]
  )
}

# Parse face (building/road) data from XML content
parse_faces <- function(xml) {
  features     <- xml2::xml_find_all(xml, ".//rcr:building|.//rcr:road")
  faces        <- xml2::xml_find_all(features, ".//gml:Face")
  direct_edges <- xml2::xml_find_all(faces, ".//gml:directedEdge")

  ids        <- xml2::xml_attr(features, "id")
  group_ids  <- rep(ids, xml2::xml_length(faces))
  geometries <- split(data.frame(
    orientation = xml2::xml_attr(direct_edges, "orientation"),
    edge_hrefs  = extract_href_value(direct_edges),
    neighbour   = xml2::xml_attr(direct_edges, "neighbour")
  ), group_ids)
  data.frame(
    id    = ids,
    type  = xml2::xml_name(features),
    edges = I(geometries[ids])
  )
}

# Create an sf object (POINTs) for nodes from node data
create_node_sf <- function(nodes, scale_data) {
  if (scale_data) {
    x <- nodes$x * 1000
    y <- nodes$y * 1000
    x <- x - min(x)
    y <- y - min(y)
  } else {
    x <- nodes$x
    y <- nodes$y
  }

  sf::st_as_sf(
    data.frame(id = nodes$id, x = x, y = y),
    coords = c("x", "y"),
    crs    = NA,
    agr    = "constant",
    remove = FALSE
  )
}

# Create an sf object (LINESTRINGs) for nodes from edge data
create_edge_sf <- function(edges, node_sf) {
  source_nodes <- node_sf$geometry[match(edges$start_node_id, node_sf$id)]
  target_nodes <- node_sf$geometry[match(edges$end_node_id, node_sf$id)]
  geometries   <- mapply(
    function(p1, p2) sf::st_linestring(c(p1, p2)),
    source_nodes, target_nodes, SIMPLIFY = FALSE
  )

  sf::st_sf(
    edges,
    geometry = geometries,
    crs      = NA,
    agr      = "constant"
  )
}

# Create an sf object (POLYGONs) for nodes from face data
create_face_sf <- function(faces, edge_sf) {
  geometries <- sapply(faces$edges, function(face) {
    edge_ids <- face$edge_hrefs
    lines    <- edge_sf$geometry[match(edge_ids, edge_sf$id)]

    sf::st_collection_extract(
      sf::st_polygonize(sf::st_union(lines)),
      "POLYGON"
    )
  })

  sf::st_sf(
    faces,
    geometry = geometries,
    crs      = NA,
    agr      = "constant"
  )
}

#' @rdname read_rrs_map
#' @export
read_rrs_map <- function(gml, scale_data = FALSE) {
  # Load the XML structure from the GML file
  xml <- xml2::read_xml(gml)

  # Extract raw data from XML structure
  nodes <- parse_nodes(xml)
  edges <- parse_edges(xml)
  faces <- parse_faces(xml)

  # Create sf Objects
  node_sf <- create_node_sf(nodes, scale_data)
  edge_sf <- create_edge_sf(edges, node_sf)
  face_sf <- create_face_sf(faces, edge_sf)

  rrs_map <- list(
    nodes = node_sf,
    edges = edge_sf,
    faces = face_sf
  )
  class(rrs_map) <- c("rrs_map", "list")

  return(rrs_map)
}
