# Morph raw map data into a road network

`morph_road_network()` converts map data from the RoboCupRescue
Simulation into a road network.

## Usage

``` r
morph_road_network(map, mode = c("all", "roads_only", "roads_no_entrances"))
```

## Arguments

- map:

  `rrs_map` data, the GML map data loaded by the
  [`read_rrs_map()`](https://nononoexe.github.io/rrstools/reference/read_rrs_map.md).

- mode:

  A character string specifying the mode of the road network:

  - `"all"`: All areas are included in the network (default).

  - `"roads_only"`: Only road areas are included.

  - `"roads_no_entrances"`: Only road areas are included, excluding
    entrances.

  When `mode` is set to `"roads_only"` or `"roads_no_entrances"`,
  passages (connections between buildings) are excluded from the network
  to prevent the formation of isolated nodes.

## Value

A list containing the network components:

- `nodes`: A sf object of nodes with point geometries.

- `edges`: A sf object of edges with linestring geometries.

## Details

This road network consists of nodes and edges. Nodes are defined as the
centroids of all areas and the center points the boundaries between
areas. Edges connect nodes between an agent can travel.

## Examples

``` r
gml <- system.file("extdata", "map-test.gml", package = "rrstools")
map <- read_rrs_map(gml)
net <- morph_road_network(map)
net
#> $nodes
#> Simple feature collection with 197 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -21 ymin: -15 xmax: 120.7273 ymax: 110.9911
#> CRS:           NA
#> First 10 features:
#>     id origin_id     type degree                  geometry
#> 1  977       956 building      1           POINT (90 19.5)
#> 2  978       957 building      1           POINT (90 13.5)
#> 3  979       958 building      1            POINT (90 7.5)
#> 4  980       959 building      1             POINT (65 16)
#> 5  981       952 building      1           POINT (90 37.5)
#> 6  982       953 building      1 POINT (65.08122 46.75888)
#> 7  983       954 building      1           POINT (90 31.5)
#> 8  984       955 building      1           POINT (90 25.5)
#> 9  985       948 building      1           POINT (90 61.5)
#> 10 986       949 building      1           POINT (90 55.5)
#> 
#> $edges
#> Simple feature collection with 204 features and 4 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -21 ymin: -15 xmax: 120.7273 ymax: 110.9911
#> CRS:           NA
#> First 10 features:
#>      id     type from  to                       geometry
#> 1  1174 building  956 772    LINESTRING (90 19.5, 95 18)
#> 2  1175 building  957 788    LINESTRING (90 13.5, 95 15)
#> 3  1176 building  958 793      LINESTRING (90 7.5, 95 8)
#> 4  1177 building  959 806     LINESTRING (65 16, 65.5 7)
#> 5  1178 building  952 752    LINESTRING (90 37.5, 95 39)
#> 6  1179 building  953 818 LINESTRING (65.08122 46.758...
#> 7  1180 building  954 754    LINESTRING (90 31.5, 95 30)
#> 8  1181 building  955 770    LINESTRING (90 25.5, 95 27)
#> 9  1182 building  948 709    LINESTRING (90 61.5, 95 60)
#> 10 1183 building  949 725    LINESTRING (90 55.5, 95 57)
#> 
#> attr(,"class")
#> [1] "rrs_road_network" "list"            
```
