#' Download RoboCupRescue Simulation scenarios
#'
#' `download_scenarios` downloads scenario archives for the RoboCupRescue
#' Simulation from the RCRS Scenario Hub. It allows filtering by scenario name.
#'
#' @param names A character vector of scenario names to download (e.g., "kobe").
#' @param download_dir A string specifying the directory where files will be
#'   downloaded. If the directory does not exist, it will be created.
#' @param extract A logical value. If `TRUE`, downloaded archives will be
#'   extracted into the `download_dir`, and the original archive file will be
#'   removed upon successful extraction.
#'   Supported formats: `.zip` (extracted using [utils::unzip()]),
#'   `.7z` (requires 7-zip command-line tool (`7z`) to be installed and
#'   available in the system's PATH).
#'   Defaults to `FALSE`.
#' @return Invisibly returns a character vector of the local paths corresponding
#'   to the results of the process. If extraction is successful, the path to
#'   the newly created data directory is returned; otherwise, the path to the
#'   downloaded archive file is returned. Returns an empty character vector if
#'   no files were downloaded.
#' @export
#' @examples
#' \donttest{
#' # Download the kobe scenario
#' download_scenarios(names = "kobe", download_dir = tempdir())
#'
#' # Download and extract the kobe scenario
#' download_scenarios(names = "kobe", download_dir = tempdir(), extract = TRUE)
#' }
download_scenarios <- function(names,
                               download_dir,
                               extract = FALSE) {
  # Ensure the download directory exists, create it if not
  if (!dir.exists(download_dir)) {
    message(paste("Creating directory:", download_dir))
    dir.create(download_dir, recursive = TRUE)
  }

  # Remove duplicate names
  names <- unique(names)

  # Get the scenario list
  scenarios <- get_scenarios()

  # Filter by name
  scenarios <- scenarios[scenarios$name %in% names, ]

  # Check if any scenarios matched the criteria
  if (nrow(scenarios) == 0) {
    message("No scenarios found for the specified names.")
    return(invisible(character(0)))
  }

  # Process each scenario
  n <- nrow(scenarios)
  message(paste("Found", n, "scenario(s) to process."))
  results <- lapply(seq_len(n), function(i) {
    dest_path <- download_scenario(
      url          = scenarios$download_url[i],
      download_dir = download_dir,
      index        = i,
      total        = n
    )
    if (extract && !is.null(dest_path)) {
      extract_scenario(dest_path = dest_path, download_dir = download_dir)
    } else {
      dest_path
    }
  })

  return(invisible(unlist(results)))
}

download_scenario <- function(url, download_dir, index, total) {
  file_name <- basename(url)
  dest_path <- file.path(download_dir, file_name)

  message(sprintf("[%d/%d] Processing %s ...", index, total, file_name))
  tryCatch({
    suppressWarnings(
      utils::download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
    )

    message(" -> Download complete.")
    dest_path
  }, error = function(e) {
    warning("Failed to download ", url, "\n",
            "Error: ", e$message)
    NULL
  })
}

extract_scenario <- function(dest_path, download_dir) {
  file_name <- basename(dest_path)

  # Determine the expected extracted directory name (remove the last extension)
  extracted_dir_name <- sub("\\.[^.]+$", "", file_name)
  extracted_path <- file.path(download_dir, extracted_dir_name)

  message(" -> Attempting to extract...")
  tryCatch({
    if (grepl("\\.zip$", file_name, ignore.case = TRUE)) {
      # Extract .zip using R's built-in unzip()
      utils::unzip(dest_path, exdir = download_dir)
      message(" -> Extraction complete.")

      message(" -> Removing archive file...")
      file.remove(dest_path)
      message(" -> Archive file removed.")

      extracted_path

    } else if (grepl("\\.7z$", file_name, ignore.case = TRUE)) {
      # Extract .7z using 7-zip command-line tool
      sevenzip_path <- Sys.which("7z")

      if (sevenzip_path == "") {
        warning("Could not extract '", file_name, "'.\n",
                "'7z' command was not found. Please install 7-zip and ",
                "ensure it is in your system's PATH.")
        return(dest_path)
      }

      args <- c("x", shQuote(dest_path),
                paste0("-o", shQuote(download_dir)))
      status <- system2(sevenzip_path,
                        args = args, stdout = FALSE, stderr = FALSE)

      if (status == 0) {
        message(" -> Extraction complete.")

        message(" -> Removing archive file...")
        file.remove(dest_path)
        message(" -> Archive file removed.")

        extracted_path
      } else {
        warning("Extraction failed for '", file_name, "'.",
                "The archive file was not removed.")
        dest_path
      }

    } else {
      warning("Extraction is not support for this file type: '",
              file_name, "'. Skipping extraction.")
      dest_path
    }
  }, error = function(e) {
    warning("Failed to extract ", dest_path, "\n",
            "Error: ", e$message)
    dest_path
  })
}
