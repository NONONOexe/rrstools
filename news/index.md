# Changelog

## rrstools 1.1.0

- Added features to interact with the [RCRS Scenario
  Hub](https://github.com/NONONOexe/rcrs-scenario-hub), including
  `download_scenario()`,
  [`get_scenarios()`](https://nononoexe.github.io/rrstools/reference/get_scenarios.md),
  and
  [`set_hub_url()`](https://nononoexe.github.io/rrstools/reference/set_hub_url.md)/[`get_hub_url()`](https://nononoexe.github.io/rrstools/reference/get_hub_url.md).
- Added comprehensive test cases for the newly introduced Scenario Hub
  functions.
- Fixed a CRAN policy compliance issue in the scenario download
  workflow; removed `download_maps()` and ensured that the output
  directory argument in the new `download_scenario()` is mandatory to
  prevent writing to the working directory without explicit user
  consent.
- Improved package documentation by updating and expanding descriptions
  in both `DESCRIPTION` and `README.md`
- Improved package maintenance by removing legacy competition map data
  and fully migration to the RCRS Scenario Hub infrastructure.

## rrstools 1.0.0

- Initial CRAN submission.
