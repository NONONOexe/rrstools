# Helper function to clear the session cache before or after tests.
# This prevent test interference and ensures a clean state.
clear_cache <- function() {
  if (exists(".rrstools_env", envir = globalenv()) || exists(".rrstools_env")) {
    .rrstools_env$scenarios_cache <- NULL
  }
}
