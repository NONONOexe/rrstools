
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rrstools <img src="man/figures/logo.png" align="right" height="138"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rrstools)](https://CRAN.R-project.org/package=rrstools)

<!-- badges: end -->

## Installation

You can install the development version of rrstools using the following
methods:

### Using `install.packages()` (R-universe)

``` r
# Enable the R-universe
options(repos = c(
  nononoexe = "https://nononoexe.r-universe.dev",
  cran = "https://cloud.r-project.org"
))

# Install the package
install.packages("rrstools")
```

### Using `pak`

``` r
# install.packages("pak")
pak::pak("nononoexe/rrstools")
```

## Usage

This package provides functions to read and plot RRS map data.

``` r
library(rrstools)

# Sample GML file
gml <- system.file("extdata", "map-test.gml", package = "rrstools")

# Read the map data from the GML file
map_data <- read_rrs_map(gml)

# Print the map data
map_data
#> $nodes
#> Simple feature collection with 315 features and 1 field
#> Attribute-geometry relationship: constant (1)
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -30 ymin: -20 xmax: 135 ymax: 121
#> CRS:           NA
#> First 10 features:
#>    id            geometry
#> 1   0 POINT (83.16 43.16)
#> 2   1 POINT (76.84 36.84)
#> 3   2  POINT (76.84 3.16)
#> 4   3  POINT (83.16 3.16)
#> 5   4   POINT (3.16 3.16)
#> 6   5 POINT (-3.16 -3.16)
#> 7   6    POINT (80 -3.16)
#> 8   7       POINT (67 43)
#> 9   8 POINT (73.16 36.84)
#> 10  9    POINT (40 -3.16)
#> 
#> $edges
#> Simple feature collection with 413 features and 1 field
#> Attribute-geometry relationship: constant (1)
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -30 ymin: -20 xmax: 135 ymax: 121
#> CRS:           NA
#> First 10 features:
#>     id                       geometry
#> 1  103 LINESTRING (76.84 36.84, 83...
#> 2  110 LINESTRING (76.84 3.16, 80 ...
#> 3  108 LINESTRING (83.16 43.16, 83...
#> 4  109 LINESTRING (-3.16 -3.16, 3....
#> 5  106 LINESTRING (76.84 3.16, 76....
#> 6  107 LINESTRING (83.16 3.16, 76....
#> 7  104 LINESTRING (103.16 60, 103....
#> 8  119 LINESTRING (80 -3.16, 40 -3...
#> 9  117 LINESTRING (42.16 103.16, 8...
#> 10 116 LINESTRING (76.84 36.84, 73...
#> 
#> $buildings
#> Simple feature collection with 37 features and 1 field
#> Attribute-geometry relationship: constant (1)
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -30 ymin: -20 xmax: 135 ymax: 121
#> CRS:           NA
#> First 10 features:
#>     id                       geometry
#> 1  956 POLYGON ((95 19, 95 17, 85 ...
#> 2  957 POLYGON ((95 16, 95 14, 95 ...
#> 3  958 POLYGON ((95 10, 95 9, 95 7...
#> 4  959 POLYGON ((68 7, 63 7, 56 7,...
#> 5  952 POLYGON ((85 40, 95 40, 95 ...
#> 6  953 POLYGON ((80 55, 80 44, 77 ...
#> 7  954 POLYGON ((95 31, 95 29, 85 ...
#> 8  955 POLYGON ((95 28, 95 26, 95 ...
#> 9  948 POLYGON ((85 64, 95 64, 95 ...
#> 10 949 POLYGON ((85 58, 95 58, 95 ...
#> 
#> $roads
#> Simple feature collection with 58 features and 1 field
#> Attribute-geometry relationship: constant (1)
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -11.2 ymin: -10 xmax: 103.16 ymax: 103.16
#> CRS:           NA
#> First 10 features:
#>     id                       geometry
#> 1  275 POLYGON ((47 27, 53 34, 67 ...
#> 2  274 POLYGON ((99.56 -3.16, 80 -...
#> 3  273 POLYGON ((-3.16 80, 3 77, 3...
#> 4  279 POLYGON ((83.16 3.16, 80 -3...
#> 5  278 POLYGON ((47 58, 45 68, 53 ...
#> 6  281 POLYGON ((3 84, 3 77, -3.16...
#> 7  280 POLYGON ((89.15 86.38, 84.1...
#> 8  258 POLYGON ((73.16 26.84, 67 3...
#> 9  259 POLYGON ((92.24 92.24, 84.1...
#> 10 256 POLYGON ((3 77, 3 84, 18 84...
#> 
#> attr(,"class")
#> [1] "rrs_map" "list"

# Plot the map data
plot(map_data)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://nononoexe.github.io/rrstools/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
