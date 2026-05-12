# Read RoboCupRescue Simulation scenario data from XML file

`read_rrs_scenario()` reads and processes scenario data for
RoboCupRescue Simulation from a XML file.

## Usage

``` r
read_rrs_scenario(xml)
```

## Arguments

- xml:

  Path to the XML file containing the scenario data.

## Value

A `rrs_scenario` object. This object is a `data.frame`, and contains the
following columns:

- `type`: Character. The type of the entity (e.g., "refuge",
  "civilian").

- `location`: Character. The entity ID of the location where the entity
  is placed.

- `attributes`: List. A list containing other attributes of the entity.

## Examples

``` r
xml <- system.file("extdata", "scenario-test.xml", package = "rrstools")
scenario <- read_rrs_scenario(xml)
scenario
#> RoboCupRescue Simulation scenario object
#> Number of entities: 35
#> Entities by type:
#>             Type Count
#>  ambulancecentre     1
#>    ambulanceteam     4
#>         civilian    20
#>      firebrigade     6
#>      firestation     1
#>      policeforce     1
#>           refuge     2
```
