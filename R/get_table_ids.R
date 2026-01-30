#' Get the unique IDs for a particular POLIS table
#'
#' @description
#' Return the full IDs available in API table.
#'
#' @param table_data `tibble` One row tibble with the associated table data.
#' @param api_key `str` POLIS API Key.
#' @returns `str` A vector of IDs.
#' @export
get_table_ids <- function(table_data, api_key = Sys.getenv("POLIS_API_KEY")) {
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

    response <- call_single_url(api_url)
    ids <- response |> dplyr::pull(table_data$polis_id)

    cli::cli_process_done()

    return(ids)
  }
