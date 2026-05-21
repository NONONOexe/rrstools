# Print a RRS scenario object

`print` prints a concise summary of a `rrs_scenario` object. It displays
the object type, the total number of entities, and a table with the
counts of entities for each type.

## Usage

``` r
# S3 method for class 'rrs_scenario'
print(x, ...)
```

## Arguments

- x:

  An object class `rrs_scenario`.

- ...:

  Not used.

## Value

Invisibly returns `x` (called for side effects).

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
