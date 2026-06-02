# Set the URL of the RCRS Scenario Hub

`set_hub_url` sets the URL of the RCRS Scenario Hub for the current
session. The cached scenario list is cleared when the URL is changed.

## Usage

``` r
set_hub_url(url)
```

## Arguments

- url:

  A string specifying the URL of the RCRS Scenario Hub.

## Value

Invisibly returns the previous URL.

## Examples

``` r
# \donttest{
set_hub_url("https://example.com/scenarios.json")
# }
```
