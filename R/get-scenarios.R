#' Get available scenarios from the RCRS Scenario Hub
#'
#' `get_scenarios` retrieves the list of available scenarios from the RCRS
#' Scenario Hub and returns it as a data frame. The result is cached within
#' the session to avoid repeated downloads. Use `refresh = TRUE` to force
#' re-fetching from the Hub.
#'
#' @param refresh A logical value. If `TRUE`, the cached data is discarded
#'   and the scenario list is re-fetched from the Hub.
#'   Defaults to `FALSE`.
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{name}{Scenario name}
#'     \item{download_url}{URL to download the map scenario archive}
#'     \item{map}{Map name}
#'     \item{city}{Name of the city that map is modeled after}
#'     \item{country}{Name of the country where the city is located}
#'     \item{lat}{Latitude of the map location. If the exact location is
#'       unknown, the center of the city is used.}
#'     \item{lon}{Longitude of the map location. If the exact location is
#'       unknown, the center of the city is used.}
#'     \item{thumbnail_url}{URL of the thumbnail image of the map}
#'   }
#' @export
#' @examples
#' \donttest{
#' # Get available scenarios
#' get_scenarios()
#'
#' # Force re-fetch from the Hub
#' get_scenarios(refresh = TRUE)
#' }
get_scenarios <- function(refresh = FALSE) {
  # Return cache if available
  if (!refresh && !is.null(.rrstools_env$scenarios_cache)) {
    return(.rrstools_env$scenarios_cache)
  }

  # Fetch scenarios.json
  message("Fetching scenario list from RCRS Scenario Hub...")

  # Initialize a variable to capture low-level warning messages
  warn_msg <- NULL

  tryCatch({
    # Catch any warning during readLines and store its message
    withCallingHandlers({
      json_text <- readLines(url(get_hub_url()), warn = FALSE)
    }, warning = function(w) {
      warn_msg <<- w$message
      invokeRestart("muffleWarning") # Suppress the warning from printing console
    })
    hub_data  <- jsonlite::fromJSON(paste(json_text, collapse = "\n"),
                                    simplifyVector = FALSE)
  }, error = function(e) {
    detailed_msg <- e$message
    if (!is.null(warn_msg)) {
      detailed_msg <- paste0(detailed_msg, " (", warn_msg, ")")
    }

    stop("Failed to fetch scenario list from RCRS Scenario Hub.\n",
         "Error: ", detailed_msg, call. = FALSE)
  })

  # Convert to data frame
  rows <- lapply(hub_data$maps, function(map) {
    lapply(map$scenarios, function(scenario) {
      data.frame(
        name          = scenario$name,
        download_url  = scenario$download_url,
        map           = map$name,
        city          = map$city,
        country       = map$country,
        lat           = map$lat,
        lon           = map$lon,
        thumbnail_url = map$thumbnail_url,
        stringsAsFactors = FALSE
      )
    })
  })
  scenarios <- do.call(rbind, unlist(rows, recursive = FALSE))

  # Save to cache
  .rrstools_env$scenarios_cache <- scenarios

  scenarios
}
