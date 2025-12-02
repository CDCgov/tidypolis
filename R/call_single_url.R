#' Call single URL
#' @description Call a return the formatted output frome one URL.
#' @param url `str` Single url.
#' @param api_key `str` validated API key.
#' @param times `int` Number of times to attempt connection with API.
#' @export
#' @returns `tibble` Dataset requested from the API.
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

  # Convert string representations of NULL/NA to actual NA values
  data <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::case_when(
          as.character(.x) %in% c("NULL", "NA", "") ~ NA_character_,
          TRUE ~ as.character(.x)
        )
      )
    )

  # Infer and apply column types (works with sparse data)
  data <- readr::type_convert(data, col_types = readr::cols())

  return(data)
  # Sys.sleep(1.25)
}
