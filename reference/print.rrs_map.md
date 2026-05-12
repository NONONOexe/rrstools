# Print a RoboCupRescue Simulation map object

`print` prints a concise summary of a `rrs_map` object. It displays the
object type, the number of nodes, edges, and faces it contains, its
overall bounding box.

## Usage

``` r
# S3 method for class 'rrs_map'
print(x, ...)
```

## Arguments

- x:

  An object class `rrs_map`.

- ...:

  Not used.

## Examples

``` r
gml <- system.file("extdata", "map-test.gml", package = "rrstools")
map <- read_rrs_map(gml)
map
#> RoboCupRescue Simulation map object
#> Number of elements: nodes: 315 edges: 413 faces: 95
#> Bounding box: xmin: -30.000000 ymin: -20.000000 xmax: 135.000000 ymax: 121.000000
```
