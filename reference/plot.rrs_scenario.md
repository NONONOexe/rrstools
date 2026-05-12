# Plot a RRS scenario object (Not supported directly)

A `rrs_scenario` object cannot be plotted by itself because it lacks the
necessary spatial conext provided by a map. This function exists to
intercept calls to `plot(scenario)` and provide a helpful error message.

## Usage

``` r
# Collect usage:
# plot(map, scenario)
```

## Arguments

- x:

  An object class `rrs_scenario`.

- ...:

  Not used.

## Details

To visualize a scenario, you must plot it together with a `rrs_map`
object.
