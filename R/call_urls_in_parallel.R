# Private function

#' Helper function of call_urls_in_parallel
#' 
#' @description
#' Does the actual requests to the POLIS API. The default is set to 10 concurrent requests, 
#' which is the default of the [httr2::req_perform_parallel] function.
#' 
#' @inheritParams call_urls_in_parallel
#'
#' @returns `list` A list containing a tibble of data and a vector of odata nextLink values.
#'
#' @keywords internal
#' 
call_urls_in_parallel_helper <- function(urls, polis_key, requests_per_minute, concurrent_requests) {

  url_requests <- purrr::map(urls, \(x) {
    tryCatch(
      {
        httr2::request(x) |>
          httr2::req_headers(`Authorization-Token` = polis_key) |>
          httr2::req_throttle(capacity = requests_per_minute)
      },
      error = \(e) {
        cli::cli_alert_info(paste0("Not a valid URL and will be ignored: ", x))
        NULL
      }
    )
  })

  # Remove invalid URLs from the call (which are NULLs)
  url_requests <- purrr::compact(url_requests)

  # If all URLs are invalid, return an empty list
  if (length(url_requests) == 0) {
    return(list(data = dplyr::tibble(), next_links = character()))
  }

  response <- httr2::req_perform_parallel(url_requests, 
    on_error = "continue", 
    max_active = concurrent_requests)
  
  out <- purrr::map(response, \(x) {
    tryCatch(
      {
        httr2::resp_body_json(x, simplifyVector = TRUE)
      },
      error = \(e) {
        cli::cli_alert_info(paste0("Bad request: ", x$url))
        NULL
      }
    ) 
  }) |>
    purrr::compact()

  # the actual data
  value <- purrr::map(out, \(x) x$value) |> 
    dplyr::bind_rows() |>
    dplyr::tibble()

  # get the next links, remove the NULLs
  next_link_urls <- purrr::map(out, \(x) x$`@odata.nextLink`) |> unlist()

  return(list(data = value, next_links = next_link_urls))

}

# Main function

#' Requests data from the POLIS API in parallel
#' 
#' @description
#' Requests data from the POLIS API.
#'
#' @param urls `str` A URL string or a vector of URL strings to call to the POLIS API.
#' @param requests_per_minute `int` Maximum number of requests per minute. Defaults to 30.
#' @param polis_key `str` POLIS API key.
#' @param concurrent_requests `int` Number of concurrent requests. Defaults to 10.
#'
#' @returns `tibble` Data requested from the POLIS API. 
#'
#' @export
#' 
#' @examples
#' \dontrun{
#'    init_tidypolis() # initializes saved POLIS API key in the global environment
#'    urls <- c("https://extranet.who.int/polis/api/v2/Virus?$filter=UpdatedDate%20ge%202026-01-20T11:56:54.220Z%20and%20UpdatedDate%20le%202026-01-27T11:56:54.220Z",
#'              "https://extranet.who.int/polis/api/v2/Virus?$filter=UpdatedDate%20gt%202026-01-27T11:56:54.220Z")
#'    virus_data <- call_urls_in_parallel(urls)
#' }
call_urls_in_parallel <- function(urls, polis_key = Sys.getenv("POLIS_API_KEY"), requests_per_minute = 30, concurrent_requests = 10) {
  
  response <- call_urls_in_parallel_helper(urls, polis_key, requests_per_minute, concurrent_requests)
  api_data <- response$data
  next_link_urls <- response$next_links

  while (length(next_link_urls) != 0) {
    response <- call_urls_in_parallel_helper(next_link_urls, polis_key, requests_per_minute, concurrent_requests)
    api_data <- dplyr::bind_rows(api_data, response$data)
    next_link_urls <- response$next_links
  }

  return(api_data)

}