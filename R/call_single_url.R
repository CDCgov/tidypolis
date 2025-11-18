#' Call single URL
#' @description Call a return the formatted output frome one URL
#' @param url `str` single url
#' @param api_key `str` validated API key
#' @param times `int` number of times to attempt connection with API
#' @export
#' @returns tibble
call_single_url <- function(url,
                            api_key = Sys.getenv("POLIS_API_KEY"),
                            times = 10) {
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

  out <- jsonlite::fromJSON(rawToChar(response$content))
  data <- dplyr::as_tibble(out$value)

  # Prevent columns from becoming logical
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  return(data)
  # Sys.sleep(1.25)
}
