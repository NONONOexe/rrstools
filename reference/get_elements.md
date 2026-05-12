# Get elements from a RoboCupRescue Simulation map

These functions provide methods to extract specific geographical
elements from an `rrs_map` object. You can retrieve all elements of a
certain type or filter them by their IDs.

## Usage

``` r
get_nodes(map, ids = character())

get_edges(map, ids = character())

get_roads(map, ids = character())

get_buildings(map, ids = character())

get_entrances(map, ids = character())

get_passage(map, ids = character())
```

## Arguments

- map:

  An object of class `rrs_map`.

- ids:

  A character vector of element IDs to filter by. If empty (default),
  all elements of the specified type are returned.

## Value

An `sf` data frame containing the requested map elements.

## Examples

``` r
gml <- system.file("extdata", "map-test.gml", package = "rrstools")
map <- read_rrs_map(gml)

# Get all nodes
get_nodes(map)
#> Simple feature collection with 315 features and 3 fields
#> Attribute-geometry relationships: constant (3)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -30 ymin: -20 xmax: 135 ymax: 121
#> CRS:           NA
#> First 10 features:
#>    id     x     y            geometry
#> 1   0 83.16 43.16 POINT (83.16 43.16)
#> 2   1 76.84 36.84 POINT (76.84 36.84)
#> 3   2 76.84  3.16  POINT (76.84 3.16)
#> 4   3 83.16  3.16  POINT (83.16 3.16)
#> 5   4  3.16  3.16   POINT (3.16 3.16)
#> 6   5 -3.16 -3.16 POINT (-3.16 -3.16)
#> 7   6 80.00 -3.16    POINT (80 -3.16)
#> 8   7 67.00 43.00       POINT (67 43)
#> 9   8 73.16 36.84 POINT (73.16 36.84)
#> 10  9 40.00 -3.16    POINT (40 -3.16)

# Get a specific node by ID
get_nodes(map, c("0", "2"))
#> Simple feature collection with 2 features and 3 fields
#> Attribute-geometry relationships: constant (3)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 76.84 ymin: 3.16 xmax: 83.16 ymax: 43.16
#> CRS:           NA
#>   id     x     y            geometry
#> 1  0 83.16 43.16 POINT (83.16 43.16)
#> 3  2 76.84  3.16  POINT (76.84 3.16)
```
