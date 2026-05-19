gml <- system.file("extdata", "map-test.gml", package = "rrstools")
map <- read_rrs_map(gml)


# Return value structure --------------------------------------------------

test_that("`read_rrs_map` returns an `rrs_map` object", {
  expect_s3_class(map, "rrs_map")
  expect_s3_class(map, "list")
})

test_that("`read_rrs_map` returns a list with nodes, edges, and faces", {
  expect_named(map, c("nodes", "edges", "faces"))
})


# nodes -------------------------------------------------------------------

test_that("`read_rrs_map` returns nodes as an sf POINT object", {
  expect_s3_class(map$nodes, "sf")
  expect_all_true(sf::st_geometry_type(map$nodes) == "POINT")
})

test_that("`read_rrs_map` nodes have the correct columns", {
  expect_named(map$nodes, c("id", "x", "y", "geometry"))
})

test_that("`read_rrs_map` nodes have the expected number of rows", {
  expect_equal(nrow(map$nodes), 315L)
})


# edges -------------------------------------------------------------------

test_that("`read_rrs_map` returns edges as an sf LINESTRING object", {
  expect_s3_class(map$edges, "sf")
  expect_all_true(sf::st_geometry_type(map$edges) == "LINESTRING")
})

test_that("`read_rrs_map` edges have the correct columns", {
  expect_named(map$edges, c("id", "start_node_id", "end_node_id", "geometry"))
})

test_that("`read_rrs_map` edges have the expected number of rows", {
  expect_equal(nrow(map$edges), 413L)
})

test_that("`read_rrs_map` edge node IDs reference valid node IDs", {
  node_ids <- map$nodes$id
  expect_all_true(map$edges$start_node_id %in% node_ids)
  expect_all_true(map$edges$end_node_id   %in% node_ids)
})


# faces -------------------------------------------------------------------

test_that("`read_rrs_map` returns faces as an sf POLYGON object", {
  expect_s3_class(map$faces, "sf")
  expect_all_true(sf::st_geometry_type(map$faces) == "POLYGON")
})

test_that("`read_rrs_map` faces have the correct columns", {
  expect_named(map$faces, c("id", "type", "edges", "geometry"))
})

test_that("`read_rrs_map` faces contain both buildings and roads", {
  expect_true("building" %in% map$faces$type)
  expect_true("road"     %in% map$faces$type)
})

test_that("`read_rrs_map` faces have the expected number of rows", {
  expect_equal(nrow(map$faces), 95L)
})


# `scale_data = TRUE` -----------------------------------------------------

test_that("`read_rrs_map` with `scale_data = TRUE` scales coordinates by 1,000", {
  map_raw    <- read_rrs_map(gml, scale_data = FALSE)
  map_scaled <- read_rrs_map(gml, scale_data = TRUE)

  raw_x    <- map_raw$nodes$x
  scaled_x <- map_scaled$nodes$x

  # After scaling and shifting the min is always 0
  expect_equal(min(scaled_x), 0)
  expect_equal(min(map_scaled$nodes$y), 0)

  # Range should be 1,000x larger than the unscaled range
  expect_equal(diff(range(scaled_x)), diff(range(raw_x)) * 1000)
})

test_that("`read_rrs_map` with `scale_data = FALSE` keeps original coordinates", {
  expect_true(max(abs(map$nodes$x)) <= 135)
  expect_true(max(abs(map$nodes$y)) <= 121)
})


# CRS ---------------------------------------------------------------------

test_that("`read_rrs_map` returns sf objects without a CRS", {
  expect_true(is.na(sf::st_crs(map$nodes)))
  expect_true(is.na(sf::st_crs(map$edges)))
  expect_true(is.na(sf::st_crs(map$faces)))
})


# Error handling ----------------------------------------------------------

test_that("`read_rrs_map` throws an error for a non-existent file", {
  expect_error(read_rrs_map("non-existent.gml"))
})
