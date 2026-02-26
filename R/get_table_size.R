#' Get table size from POLIS
#'
#' @param .table `str` Table to be downloaded
#' @param api_key `str` API Key
#' @param cache_file `str` Cache file location
#' @param extra_filter `str` additional filtering parameters
#' @export
get_table_size <- function(.table,
                           api_key = Sys.getenv("POLIS_API_KEY"),
                           cache_file = Sys.getenv("POLIS_CACHE_FILE"),
                           extra_filter = "") {
  table_data <- get_polis_cache(.table = .table)

  # disable SSL Mode
  httr::set_config(httr::config(ssl_verifypeer = 0L))

  # Variables: URL, Token, Filters, ...
  polis_api_root_url <- "https://extranet.who.int/polis/api/v2/"

  api_url <-
    paste0(
      polis_api_root_url,
      table_data$endpoint,
      "?$count=true&$top=0",
      extra_filter
    )

  # response <- httr::GET(url=api_url, httr::add_headers("authorization-token" = api_key))

  response <- httr::RETRY(
    verb = "GET",
    url = api_url,
    config = httr::add_headers("authorization-token" = api_key),
    times = 10,
    pause_min = 2,
    quiet = TRUE,
    terminate_on_success = TRUE
  )

  out <- jsonlite::fromJSON(rawToChar(response$content))

  # dplyr::as_tibble(out$value)


  table_size <- response |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  table_size <- as.integer(table_size$`@odata.count`)

  return(table_size)

}
