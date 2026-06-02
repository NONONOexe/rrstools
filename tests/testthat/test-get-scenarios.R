# Helper to clear cache before/after tests
clear_cache <- function() {
  if (exists(".rrstools_env", envir = globalenv()) || exists(".rrstools_env")) {
    .rrstools_env$scenarios_cache <- NULL
  }
}


# Return value structure --------------------------------------------------

test_that("`get_scenarios` returns a data frame with correct columns", {
  clear_cache()

  expect_message(res <- get_scenarios(), "Fetching scenario list")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2L)
  expect_named(res, c("name", "download_url", "map", "city", "country",
                      "lat", "lon", "thumbnail_url"))

  expect_equal(res$name, c("scenario_1", "scenario_2"))
  expect_equal(res$city, c("test_city", "test_city"))
})


# Caching behavior --------------------------------------------------------

test_that("`get_scenarios` caches the result and respects `refresh` argument", {
  clear_cache()

  # 1st call: Should fetch from "Hub" and save to cache
  expect_message(get_scenarios(), "Fetching scenario list")
  expect_false(is.null(.rrstools_env$scenarios_cache))

  # 2nd call (refresh = FALSE): Should use cache and output specific message
  expect_message(res_cached <- get_scenarios(refresh = FALSE), "Using cached scenario list")
  expect_s3_class(res_cached, "data.frame")

  # 3rd call (refresh = TRUE): Should force re-fetch even if cache exists
  expect_message(get_scenarios(refresh = TRUE), "Fetching scenario list")
})


# Error handling ----------------------------------------------------------

test_that("`get_scenarios` throws a clean error when fetching fails", {
  clear_cache()

  old_mock_url <- set_hub_url("file://non-exist-file.json")
  withr::defer(set_hub_url(old_mock_url))

  expect_error(
    get_scenarios(),
    regexp = "Failed to fetch scenario list from RCRS Scenario Hub"
  )
})
