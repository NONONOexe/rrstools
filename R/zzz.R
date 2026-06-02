.rrstools_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .rrstools_env$hub_url <- "https://raw.githubusercontent.com/nononoexe/rcrs-scenario-hub/main/scenarios.json"
  .rrstools_env$scenarios_cache <- NULL
}
