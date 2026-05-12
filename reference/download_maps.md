# Download RoboCupRescue Simulation maps

`download_maps` downloads map data archives for the RoboCupRescue
Simulation from the official website. It allows filtering by year and
map name.

## Usage

``` r
download_maps(
  years = 2024,
  maps = character(),
  download_dir = getwd(),
  extract = FALSE
)
```

## Arguments

- years:

  A numeric vector of competition years to download. If empty, maps from
  all available years are considered. Defaults to `2024`.

- maps:

  A character vector of map names to download (e.g., "kobe"). If empty,
  all maps for the selected years are downloaded. Defaults to an empty
  character vector.

- download_dir:

  A string specifying the directory where files will be downloaded. If
  the directory does not exist, it will be created. Defaults to the
  current working directory
  ([`getwd()`](https://rdrr.io/r/base/getwd.html)).

- extract:

  A logical value. If `TRUE`, downloaded `.7z` archives will be
  extracted into the `download_dir`, and the original archive file will
  be removed upon successful extraction. Note: This feature requires the
  7-Zip command-line tool (`7z`) to be installed and available in the
  system's PATH. Defaults to `FALSE`.

## Value

Invisibly returns a character vector of the local paths corresponding to
the results of the process. If extraction is successful, the path to the
newly created data directory is returned; otherwise, the path to the
downloaded archive file is returned. Returns an empty character vector
if no files were downloaded.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download the 2024 maps and extract them
download_maps(years = 2024, download_dir = "maps", extract = TRUE)
} # }
```
