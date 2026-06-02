# Setup a dummy zip file for download/extract testing
setup_dummy_zip <- function() {
  # Create a temporary directory for the source file
  tmp_src_dir <- tempfile("zip_src_")
  dir.create(tmp_src_dir)
  on.exit(unlink(tmp_src_dir, recursive = TRUE), add = TRUE)

  # Write a dummy file inside the source directory
  dummy_file <- file.path(tmp_src_dir, "map-test.gml")
  writeLines("test data", dummy_file)

  # Create the dummy zip path in the session's temp area
  zip_path <- tempfile("dummy_scenario_", fileext = ".zip")

  # Zip the file and return the temp zip path
  utils::zip(zip_path, files = dummy_file, flags = "-qj")

  zip_path
}

# Argument validation & Edge cases ----------------------------------------

test_that("`download_scenarios` returns empty vector when no scenario match", {
  local_hub_mock()

  # Prepare an isolated temporary directory for downloading
  dl_dir <- tempfile("dl_test_")
  dir.create(dl_dir)
  withr::defer(unlink(dl_dir, recursive = TRUE))

  # "non_existent" does not exist in mock-scenarios.json
  expect_message(
    res <- download_scenarios(names = "non_existent", download_dir = dl_dir),
    "No scenarios found for the specified names",
  )

  expect_type(res, "character")
  expect_length(res, 0L)
  # Check if directory was created as requested
  expect_all_true(dir.exists(dl_dir))
})


# Download without extraction ---------------------------------------------

test_that("`download_scenarios` downloads archive files without extraction", {
  local_hub_mock()

  dl_dir <- tempfile("dl_test_")
  withr::defer(unlink(dl_dir, recursive = TRUE))

  # Create a real dummy zip on disk to let download.file "download" it via file://
  zip_on_disk <- setup_dummy_zip()
  withr::defer(file.remove(zip_on_disk))

  # Dynamically modify the scenario cache so the download_url points to our local zip
  scenarios <- get_scenarios()
  scenarios$download_url <- paste0("file://", zip_on_disk)
  .rrstools_env$scenarios_cache <- scenarios

  # Use the first scenario name from mock data ("scenario_1")
  target_name <- scenarios$name[1]

  suppressMessages({
    expect_message(
      res <- download_scenarios(names        = target_name,
                                download_dir = dl_dir,
                                extract      = FALSE),
      "Download complete"
    )
  })

  expected_file <- file.path(dl_dir, basename(zip_on_disk))
  expect_equal(res, expected_file)
  expect_true(file.exists(expected_file))
})


# Download with extraction ------------------------------------------------

test_that("`download_scenarios` extracts ZIP archive and remove the original", {
  local_hub_mock()

  dl_dir <- tempfile("dl_test_")
  withr::defer(unlink(dl_dir, recursive = TRUE))

  zip_on_disk <- setup_dummy_zip()
  withr::defer(file.remove(zip_on_disk))

  scenarios <- get_scenarios()
  scenarios$download_url <- paste0("file://", zip_on_disk)
  .rrstools_env$scenarios_cache <- scenarios

  target_name <- scenarios$name[1]

  suppressMessages({
    expect_message(
      res <- download_scenarios(names        = target_name,
                                download_dir = dl_dir,
                                extract      = TRUE),
      "Extraction complete"
    )
  })

  # Expected extracted directory name (zip extension removed)
  expected_dir_name <- sub("\\.[^.]+$", "", basename(zip_on_disk))
  expected_dir <- file.path(dl_dir, expected_dir_name)
  expect_equal(res, expected_dir)

  # The temporary archive copy in dl_dir should be removed after extraction
  expect_false(file.exists(file.path(dl_dir, basename(zip_on_disk))))
})


# Error / Warning handling ------------------------------------------------
test_that("`download_scenarios` triggers warning when download fails", {
  local_hub_mock()

  dl_dir <- tempfile("dl_test_")
  withr::defer(unlink(dl_dir, recursive = TRUE))

  scenarios <- get_scenarios()
  scenarios$download_url <- "file://non-existent-archive.zip"
  .rrstools_env$scenarios_cache <- scenarios

  target_name <- scenarios$name[1]

  suppressMessages({
    expect_warning(
      res <- download_scenarios(names = target_name, download_dir = dl_dir),
      "Failed to download"
    )
  })

  expect_null(res)
})
