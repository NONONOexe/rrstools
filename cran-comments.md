## Test environments

* local: Windows 11 x64, R 4.5.2
* win-builder: R-release (R 4.6.0)
* R-hub: ubuntu-latest, R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission

## Changes since last submission

* Added functions to interact with the RCRS Scenario Hub   (`download_scenarios()`, `get_scenarios()`, `set_hub_url()`, `get_hub_url()`).
* Fixed a CRAN policy compliance issue: removed `download_maps()` and made the   output directory argument in `download_scenarios()` mandatory to prevent writing to the working directory without explicit user consent.
* Improved package documentation in `DESCRIPTION` and `README.md`.
* Removed legacy competition map data and migrated to the RCRS Scenario Hub infrastructure.

## Downstream dependencies

None
