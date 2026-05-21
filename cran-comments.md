## Test environments
* local: Windows 11 x64, R 4.5.2
* win-builder: R-release (R 4.6.0)
* R-hub: ubuntu-latest, R-devel

## R CMD check results
0 errors | 0 warnings | 1 note

* This is a new submission.
* NOTE: "Possibly misspelled words: RoboCupRescue"
  RoboCupRescue is a proper noun referring to the name of the simulation
  that this package is designed to analyze.
* NOTE: "unable to verify current time" (local only)
  This NOTE occurs because R CMD check could not connect to the external
  time API used to verify file timestamps. This is unrelated to the package.

## Downstream dependencies
None
