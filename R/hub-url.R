#' Set the URL of the RCRS Scenario Hub
#'
#' `set_hub_url` sets the URL of the RCRS Scenario Hub for the current session.
#' The cached scenario list is cleared when the URL is changed.
#'
#' @param url A string specifying the URL of the RCRS Scenario Hub.
#' @return Invisibly returns the previous URL.
#' @export
#' @examples
#' \donttest{
#' set_hub_url("https://example.com/scenarios.json")
#' }
set_hub_url <- function(url) {
  old <- .rrstools_env$hub_url
  .rrstools_env$hub_url <- url
  .rrstools_env$scenario_cache <- NULL
  invisible(old)
}

#' Get the URL of the RCRS Scenario Hub
#'
#' `get_hub_url` returns the current URL of the RCRS Scenario Hub.
#'
#' @return A string of the current URL of the RCRS Scenario Hub.
#' @export
#' @examples
#' get_hub_url()
get_hub_url <- function() {
  .rrstools_env$hub_url
}
