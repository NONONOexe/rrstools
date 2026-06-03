# Get available scenarios from the RCRS Scenario Hub

`get_scenarios` retrieves the list of available scenarios from the RCRS
Scenario Hub and returns it as a data frame. The result is cached within
the session to avoid repeated downloads. Use `refresh = TRUE` to force
re-fetching from the Hub.

## Usage

``` r
get_scenarios(refresh = FALSE)
```

## Arguments

- refresh:

  A logical value. If `TRUE`, the cached data is discarded and the
  scenario list is re-fetched from the Hub. Defaults to `FALSE`.

## Value

A data frame with the following columns:

- name:

  Scenario name

- download_url:

  URL to download the map scenario archive

- map:

  Map name

- city:

  Name of the city that map is modeled after

- country:

  Name of the country where the city is located

- lat:

  Latitude of the map location. If the exact location is unknown, the
  center of the city is used.

- lon:

  Longitude of the map location. If the exact location is unknown, the
  center of the city is used.

- thumbnail_url:

  URL of the thumbnail image of the map

## Examples

``` r
# \donttest{
# Get available scenarios
get_scenarios()
#>         name
#> 1     berlin
#> 2  eindhoven
#> 3   istanbul
#> 4       joao
#> 5       kobe
#> 6   montreal
#> 7         ny
#> 8      paris
#> 9      sakae
#> 10        sf
#>                                                                          download_url
#> 1     https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/berlin.zip
#> 2  https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/eindhoven.zip
#> 3   https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/istanbul.zip
#> 4       https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/joao.zip
#> 5       https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/kobe.zip
#> 6   https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/montreal.zip
#> 7         https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/ny.zip
#> 8      https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/paris.zip
#> 9      https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/sakae.zip
#> 10        https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/sf.zip
#>          map          city       country      lat         lon
#> 1     berlin        Berlin       Germany 52.51639   13.377500
#> 2  eindhoven     Eindhoven    Nederlands 51.44315    5.479753
#> 3   istanbul      Istanbul        Turkey 41.01224   28.976017
#> 4       joao   João Pessoa        Brazil -7.12000  -34.880000
#> 5       kobe          Kobe         Japan 34.69000  135.195556
#> 6   montreal      Montreal        Canada 45.50889  -73.554167
#> 7         ny      New York United States 40.71278  -74.006111
#> 8      paris         Paris        France 48.85670    2.352200
#> 9      sakae         Sakae         Japan 35.16734  136.904256
#> 10        sf San Francisco United States 37.77750 -122.416389
#>                                                                                       thumbnail_url
#> 1     https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/berlin/thumbnail.png?raw=true
#> 2  https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/eindhoven/thumbnail.png?raw=true
#> 3   https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/istanbul/thumbnail.png?raw=true
#> 4       https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/joao/thumbnail.png?raw=true
#> 5       https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/kobe/thumbnail.png?raw=true
#> 6   https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/montreal/thumbnail.png?raw=true
#> 7         https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/ny/thumbnail.png?raw=true
#> 8      https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/paris/thumbnail.png?raw=true
#> 9      https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/sakae/thumbnail.png?raw=true
#> 10        https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/sf/thumbnail.png?raw=true

# Force re-fetch from the Hub
get_scenarios(refresh = TRUE)
#> Fetching scenario list from RCRS Scenario Hub...
#>         name
#> 1     berlin
#> 2  eindhoven
#> 3   istanbul
#> 4       joao
#> 5       kobe
#> 6   montreal
#> 7         ny
#> 8      paris
#> 9      sakae
#> 10        sf
#>                                                                          download_url
#> 1     https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/berlin.zip
#> 2  https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/eindhoven.zip
#> 3   https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/istanbul.zip
#> 4       https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/joao.zip
#> 5       https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/kobe.zip
#> 6   https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/montreal.zip
#> 7         https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/ny.zip
#> 8      https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/paris.zip
#> 9      https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/sakae.zip
#> 10        https://github.com/NONONOexe/rcrs-scenarios/releases/latest/download/sf.zip
#>          map          city       country      lat         lon
#> 1     berlin        Berlin       Germany 52.51639   13.377500
#> 2  eindhoven     Eindhoven    Nederlands 51.44315    5.479753
#> 3   istanbul      Istanbul        Turkey 41.01224   28.976017
#> 4       joao   João Pessoa        Brazil -7.12000  -34.880000
#> 5       kobe          Kobe         Japan 34.69000  135.195556
#> 6   montreal      Montreal        Canada 45.50889  -73.554167
#> 7         ny      New York United States 40.71278  -74.006111
#> 8      paris         Paris        France 48.85670    2.352200
#> 9      sakae         Sakae         Japan 35.16734  136.904256
#> 10        sf San Francisco United States 37.77750 -122.416389
#>                                                                                       thumbnail_url
#> 1     https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/berlin/thumbnail.png?raw=true
#> 2  https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/eindhoven/thumbnail.png?raw=true
#> 3   https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/istanbul/thumbnail.png?raw=true
#> 4       https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/joao/thumbnail.png?raw=true
#> 5       https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/kobe/thumbnail.png?raw=true
#> 6   https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/montreal/thumbnail.png?raw=true
#> 7         https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/ny/thumbnail.png?raw=true
#> 8      https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/paris/thumbnail.png?raw=true
#> 9      https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/sakae/thumbnail.png?raw=true
#> 10        https://github.com/NONONOexe/rcrs-scenarios/blob/main/scenarios/sf/thumbnail.png?raw=true
# }
```
