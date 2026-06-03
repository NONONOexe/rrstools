# Download RoboCupRescue Simulation scenarios

`download_scenarios` downloads scenario archives for the RoboCupRescue
Simulation from the RCRS Scenario Hub. It allows filtering by scenario
name.

## Usage

``` r
download_scenarios(names, download_dir, extract = FALSE)
```

## Arguments

- names:

  A character vector of scenario names to download (e.g., "kobe").

- download_dir:

  A string specifying the directory where files will be downloaded. If
  the directory does not exist, it will be created.

- extract:

  A logical value. If `TRUE`, downloaded archives will be extracted into
  the `download_dir`, and the original archive file will be removed upon
  successful extraction. Supported formats: `.zip` (extracted using
  [`utils::unzip()`](https://rdrr.io/r/utils/unzip.html)), `.7z`
  (requires 7-zip command-line tool (`7z`) to be installed and available
  in the system's PATH). Defaults to `FALSE`.

## Value

Invisibly returns a character vector of the local paths corresponding to
the results of the process. If extraction is successful, the path to the
newly created data directory is returned; otherwise, the path to the
downloaded archive file is returned. Returns an empty character vector
if no files were downloaded.

## Examples

``` r
# \donttest{
# Download the kobe scenario
download_scenarios(names = "kobe", download_dir = tempdir())
#> Fetching scenario list from RCRS Scenario Hub...
#> Found 1 scenario(s) to process.
#> Processing kobe.zip ...
#>  -> Downloading...
#>  -> Download complete.

# Download and extract the kobe scenario
download_scenarios(names = "kobe", download_dir = tempdir(), extract = TRUE)
#> Fetching scenario list from RCRS Scenario Hub...
#> Found 1 scenario(s) to process.
#> Processing kobe.zip ...
#>  -> Downloading...
#>  -> Download complete.
#>  -> Attempting to extract...
#>  -> Extraction complete.
#>  -> Removing archive file...
#>  -> Archive file removed.
# }
```
