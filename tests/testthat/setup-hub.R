# Switch the Hub URL to the local mock JSON file for all tests.
old_url <- set_hub_url(paste0("file://", test_path("mock-scenarios.json")))

# Automatically restore the original production URL after all tests complete
# to prevent side effects in the development environment.
withr::defer(set_hub_url(old_url), teardown_env())
