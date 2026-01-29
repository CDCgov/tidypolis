#' Call single URL
#'
#' @description
#' Call a return the formatted output from one URL.
#'
#' @details
#' The function is designed for API endpoints that support skiptokens argument.
#'
#' @param url `str` Single url.
#' @param api_key `str` validated API key.
#' @param times `int` Number of times to attempt connection with API.
#' @param diagnostics `logical` Logging messages to see if calls are happening.
#' @returns `tibble` Data from the response.
#' @export
#' @examples
#' \dontrun{
#' call_single_url("https://extranet.who.int/polis/api/v2/Virus?$filter=VirusDate%20gt%202025-01-01")
#' }
#'
call_single_url <- function(url,
                            api_key = Sys.getenv("POLIS_API_KEY"),
                            times = 10,
                            diagnostics = FALSE) {
  # disable SSL Mode
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  # response <- httr::GET(url=url, httr::add_headers("authorization-token" = api_key))
  response <- httr::RETRY(
    verb = "GET",
    url = url,
    config = httr::add_headers("authorization-token" = api_key),
    times = times,
    quiet = TRUE,
    terminate_on_success = TRUE
  )

  if (response$status_code != 200) {
    cli::cli_alert_warning(paste0("The response has a status code of: ", response$status_code))
    cli::cli_abort("Error in pulling data from API using the URL request.")
  }

  out <- jsonlite::fromJSON(rawToChar(response$content))

  value <- dplyr::as_tibble(out$value)
  nextLink <- out$`@odata.nextLink`

  while (!is.null(nextLink)) {
    response <- httr::RETRY(
      verb = "GET",
      url = nextLink,
      config = httr::add_headers("authorization-token" = api_key),
      times = times,
      quiet = TRUE,
      terminate_on_success = TRUE
    )

    out <- jsonlite::fromJSON(rawToChar(response$content))
    value <- dplyr::bind_rows(value, dplyr::as_tibble(out$value))
    nextLink <- out$`@odata.nextLink`

    has_skip_token <- stringr::str_detect(nextLink, "skiptoken")

    if (!has_skip_token) {
      cli::cli_abort(paste0("The nextLink URL doesn't contain a skip token.",
                            " Note that as of POLIS API v3.0.0, the skip parameter is deprecated.",
                            " Please contact the POLIS team at polis@who.int."))
    }

    if (diagnostics) {
      cli::cli_alert_info("Success, moving to next page...")
    }
  }

  return(value)

}
