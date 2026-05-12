# Read RoboCupRescue Simulation map data from GML file

This function reads and processes map data for RoboCupRescue Simulation
from a GML file. It extracts nodes, edges, buildings and roads.

## Usage

``` r
read_rrs_map(gml, scale_data = FALSE)
```

## Arguments

- gml:

  Path to the GML file.

- scale_data:

  Logical. If `TRUE`, coordinates are scaled up by a factor of 1000 to
  match the simulation environment, and adjusted such that the minimum x
  and y values are 0. (Default: `TRUE`)

## Value

A list of `sf` objects: nodes, edges, buildings, and roads.

## Examples

``` r
gml <- system.file("extdata", "map-test.gml", package = "rrstools")
map <- read_rrs_map(gml)
map
#> RoboCupRescue Simulation map object
#> Number of elements: nodes: 315 edges: 413 faces: 95
#> Bounding box: xmin: -30.000000 ymin: -20.000000 xmax: 135.000000 ymax: 121.000000
```
