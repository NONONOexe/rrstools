#' Download RoboCupRescue Simulation maps
#'
#' `download_maps` downloads map data archives for the RoboCupRescue Simulation
#' from the official website. It allows filtering by year and map name.
#'
#' @param years A numeric vector of competition years to download.
#'   If empty, maps from all available years are considered.
#'   Defaults to `2024`.
#' @param maps A character vector of map names to download (e.g., "kobe").
#'   If empty, all maps for the selected years are downloaded.
#'   Defaults to an empty character vector.
#' @param download_dir A string specifying the directory where files will be
#'   downloaded. If the directory does not exist, it will be created.
#'   Defaults to the current working directory (`getwd()`).
#' @param extract A logical value. If `TRUE`, downloaded `.7z` archives
#'   will be extracted into the `download_dir`, and the original archive file
#'   will be removed upon successful extraction.
#'   Note: This feature requires the 7-Zip command-line tool (`7z`)
#'   to be installed and available in the system's PATH. Defaults to `FALSE`.
#' @return Invisibly returns a character vector of the local paths to the
#'   successfully downloaded files. Returns an empty character vector if
#'   no files were downloaded.
#' @export
#' @examples
#' \dontrun{
#' # Download the 2024 maps and extract them
#' download_maps(years = 2024, download_dir = "maps", extract = TRUE)
#' }
download_maps <- function(years        = 2024,
                          maps         = character(),
                          download_dir = getwd(),
                          extract      = FALSE) {

  # Ensure the download directory exists, create it if not
  if (!dir.exists(download_dir)) {
    message(paste("Creating directory:", download_dir))
    dir.create(download_dir, recursive = TRUE)
  }

  # Filter the map data based on user input
  utils::data("competition_maps", package = "rrstools")
  filtered_maps <- competition_maps
  if (!is.null(years) && 0 < length(years)) {
    filtered_maps <- filtered_maps[competition_maps$year %in% years, ]
  }
  if (!is.null(maps) && 0 < length(maps)) {
    filtered_maps <- filtered_maps[competition_maps$map %in% maps, ]
  }

  # Get the final list of URLs to download
  urls_to_download <- filtered_maps$url

  # Check if any maps matched the criteria
  if (length(urls_to_download) == 0) {
    message("No maps found for the specified criteria.")
    return(invisible(character(0)))
  }

  # Process each URL using the helper function
  message(paste("Found", length(urls_to_download), "map(s) to process."))
  successful_downloads <- lapply(urls_to_download, download_and_extract_single,
                                 download_dir = download_dir,
                                 extract      = extract)

  # Collect the paths of successfully downloaded files
  download_files <- unlist(successful_downloads)

  # Return the paths of download files invisibly
  return(invisible(download_files))
}

download_and_extract_single <- function(url, download_dir, extract) {
  file_name <- basename(url)
  dest_path <- file.path(download_dir, file_name)

  message(paste("Processing", file_name, "..."))

  # Download the file
  tryCatch({
    message(" -> Downloading...")
    utils::download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
    message(" -> Download complete.")

    # Extract the archive if requested
    if (extract) {
      message(" -> Attempting to extract...")
      # Check if the file is a .7z archive
      if (grepl("\\.7z$", file_name, ignore.case = TRUE)) {
        sevenzip_path <- Sys.which("7z")

        if (sevenzip_path != "") {
          # Construct and execute the 7z command
          args <- c("x", shQuote(dest_path),
                    paste0("-o", shQuote(download_dir)))
          status <- system2(sevenzip_path,
                            args = args, stdout = FALSE, stderr = FALSE)

          # Check if extraction was successful (status 0 means success)
          if (status == 0) {
            message(" -> Extraction complete.")

            # Remove the original archive file
            message(" -> Removing archive file...")
            file.remove(dest_path)
            message(" -> Archive file removed.")
          } else {
            warning("Extraction failed for '", file_name, "'. ",
                    "The archive file was not removed.")
          }
        } else {
          # Inform the user if 7z command is not found
          warning("Could not extract '", file_name, "'. \n",
                  "'7z' command was not found. Please install 7-zip and ",
                  "ensure it is in your system's PATH.")
        }
      } else {
        # Warn if the file type is not supported for extraction
        warning("Extraction is not supported for this file type: '",
                file_name, "'. Skipping extraction.")
      }
    }
    # Return the destination path on success
    return(dest_path)
  }, error = function(e) {
    warning("Failed to download or process ", url, "\n",
            "Error: ", e$message)
    # Return NULL on failure
    return(NULL)
  })
}
