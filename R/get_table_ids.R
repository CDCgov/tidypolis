#' Get the unique IDs for a particular POLIS table
#'
#' @description
#' Return the full IDs available in API table.
#'
#' @param table_data `tibble` One row tibble with the associated table data.
#' @param api_key `str` POLIS API Key.
#' @param parallel_calls `str` Whether to get table IDs using parallel calls.
#' @returns `str` A vector of IDs.
#' @export
get_table_ids <- function(table_data, api_key = Sys.getenv("POLIS_API_KEY"), parallel_calls = TRUE) {
    cli::cli_process_start(paste0("Downloading ", table_data$table, " table IDs"))

    # disable SSL Mode
    httr::set_config(httr::config(ssl_verifypeer = 0L))

    # Variables: URL, Token, Filters, ...
    polis_api_root_url <- "https://extranet.who.int/polis/api/v2/"

    api_url <-
      paste0(
        polis_api_root_url,
        table_data$endpoint,
        "?$select=Id,",
        table_data$polis_id
      )

    if (parallel_calls == FALSE) {
      response <- call_single_url(api_url)
    } else {
      table_data$polis_update_value[1] <- NA # force to download from 2000
      urls <- create_table_urls(api_url, table_data, 730)
      response <- call_urls(urls)
    }

    ids <- response |> dplyr::pull(table_data$polis_id)

    cli::cli_process_done()

    return(ids)
  }
