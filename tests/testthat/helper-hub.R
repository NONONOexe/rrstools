# Helper: temporarily active the mock Hub URL and warm the scenario cache.
# Use this in any test calls get_scenarios() or download_scenarios().
#
# Usage:
#   local_hub_mock()                   # mock URL + cache pre-warmed (default)
#   local_hub_mock(warm_cache = FALSE) # mock URL only (for get_scenario tests)
local_hub_mock <- function(warm_cache = TRUE, env = parent.frame()) {
  old_url <- set_hub_url(paste0("file://", test_path("mock-scenarios.json")))
  withr::defer(set_hub_url(old_url), envir = env)

  clear_cache()

  if (warm_cache) {
    suppressMessages(get_scenarios())
  }
}
