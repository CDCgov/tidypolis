# Private functions ----

#' Collate extracts into one table
#'
#' @description
#' This function collates the extracts into one table.
#'
#' @inheritParams update_polis_cache
#'
#' @returns `conn` A parquet connection.
#' @keywords internal
#'
collate_file_extracts <- function(table_data) {
  extract_table_folder <- file.path(Sys.getenv("POLIS_DATA_CACHE"),
                                    "raw_extracts", table_data$table)

  file_extracts <- tidypolis_io(io = "list", file_path = extract_table_folder)

  if (length(file_extracts) == 0) {
    cli::cli_abort("No table extracts to collate!")
  }

  if (as.logical(Sys.getenv("POLIS_EDAV_FLAG"))) {

    # Download files locally in the temp directory first
    dest <- tempdir()
    local_pq <- file.path(dest, table_data$table)
    AzureStor::storage_multidownload(sirfunctions::get_azure_storage_connection(),
                                     src = paste0("GID/PEB/SIR/", extract_table_folder, "/*"),
                                     dest = local_pq,
                                     recursive = TRUE,
                                     overwrite = TRUE)

    pq_connection <- arrow::open_dataset(local_pq)

  } else {

    pq_connection <- arrow::open_dataset(extract_table_folder)

  }

  return(pq_connection)

}

#' Create an extract file of an API table
#'
#' @description
#' Create an extract file of an API table and put it in the extract
#' folder of the API table. The extract only contains new data that were updated
#' since the last API pull.
#'
#' @inheritParams update_polis_table
#' @param extract `tibble` Newly updated data for the table specified.
#'
#' @returns `NULL` upon success
#' @keywords internal
#'
create_extract_file <- function(table_data, extract) {

  # Add extract to the extract table folder
  utc_time_stamp <- as.POSIXct(Sys.time(), tz = "UTC")
  utc_time_stamp <- format(utc_time_stamp, "%Y%m%dT%H%M%SZ")

  extract_name <- paste0(utc_time_stamp,"_", table_data$table, ".parquet")
  extract_table_folder <- file.path(Sys.getenv("POLIS_DATA_CACHE"), "raw_extracts", table_data$table)
  tidypolis_io(extract, "write", file.path(extract_table_folder, extract_name))

}

#' Should full table be downloaded?
#'
#' @param id_error `logical` Whether the ID column is function.
#' @param table_data `tibble` One row tibble with the data for a specific table.
#'
#' @returns `logical` Whether to download the full table or not.
#' @keywords internal
#'
get_full_table <- function(id_error, table_data) {
  # Flags during download
  if (id_error) {
    cli::cli_alert_info(
      paste0(
        table_data$endpoint,
        " has been downloaded before but the ID API is not functional, downloading all data...checking size..."
      )
    )

    return(TRUE)

  } else if (is.na(table_data$polis_update_id)) {
    cli::cli_alert_info(
      paste0(
        table_data$endpoint,
        " does not have a unique timestamps for the update, the entire table must be downloaded..."
      )
    )

    return(TRUE)

  } else if (is.na(table_data$last_sync)) {
    cli::cli_alert_info(
      paste0(
        table_data$endpoint,
        " has not been downloaded before...checking size..."
      )
    )

    return(TRUE)
  } else {
    cli::cli_alert_success(paste0("Updating the ", table_data$endpoint, " table."))
    return(FALSE)
  }
}

#' Update the POLIS table
#'
#' @description
#' Will download data above the update date recorded in the metadata cache.
#'
#' @param table_data `tibble` One row tibble with the data for a specific table.
#' @param table_url `str` URL of the endpoint for the specific table.
#' @inheritParams get_table_data
#'
#' @returns `NULL`
#' @keywords internal
#'
update_polis_table <- function(table_data, table_url, parallel_calls = TRUE) {

  # If there's a collated RDS file but the extracts folder is empty, create an extract for it.

  collated_table_name <- paste0(Sys.getenv("POLIS_DATA_CACHE"), "/", table_data$table, ".parquet")
  extract_table_folder <- file.path(Sys.getenv("POLIS_DATA_CACHE"), "raw_extracts", table_data$table)
  extract_table_files <-  tidypolis_io(io = "list", file_path = extract_table_folder)
  collated_table_exists <- tidypolis_io(io = "exists.file", file_path = collated_table_name)

  if (length(extract_table_files) == 0 && collated_table_exists) {

    cli::cli_process_start("Converting previous data cache into a parquet extract")
    utc_time_stamp <- as.POSIXct(Sys.time(), tz = "UTC")
    utc_time_stamp <- format(utc_time_stamp, "%Y%m%dT%H%M%SZ")
    old_cache <- tidypolis_io(io = "read", file_path = collated_table_name)
    extract_name <- file.path(extract_table_folder, paste0(utc_time_stamp,
                                                           "_", table_data$table,
                                                           "_from_prev_cache.parquet"))

    tidypolis_io(old_cache, io = "write", file_path = extract_name)
    cli::cli_process_done()
  }

  # Turn old RDS cache into parquet
  rds_table_path <- file.path(Sys.getenv("POLIS_DATA_CACHE"), paste0(table_data$table, ".rds"))
  rds_table_exists <- tidypolis_io(io = "exists.file",
                                   file_path = rds_table_path)

  if (rds_table_exists) {

    cli::cli_process_start("Archiving rds table")
    rds_archive_exists <- tidypolis_io(io = "exists.dir",
                                       file_path = file.path(Sys.getenv("POLIS_DATA_CACHE"),
                                                             "rds_archive"))

    if (!rds_archive_exists) {
      cli::cli_process_start(paste0("Creating RDS archive for: ", table_data$endpoint))

    old_cache <- tidypolis_io(io = "read", file_path = rds_table_path)
    tidypolis_io(old_cache, "write", file_path = file.path(Sys.getenv("POLIS_DATA_CACHE"),
                                                           "rds_archive",
                                                           paste0(table_data$table, ".rds")))
    tidypolis_io(old_cache, "write", file_path = file.path(Sys.getenv("POLIS_DATA_CACHE"),
                                                           paste0(table_data$table, ".parquet")))
    tidypolis_io(io = "delete", file_path = rds_table_path)

    rm(old_cache)
    cli::cli_process_done()

    }

  }

  time_modifier <- paste0(
    "&$filter=",
    table_data$polis_update_id,
    " gt ",
    sub(" ", "T", as.character(table_data$polis_update_value)), "Z")

  time_modifier <- gsub(" ", "+", time_modifier)
  table_size <- get_table_size(.table = table_data$table, extra_filter = time_modifier)

    if (table_size == 0) {
      cli::cli_alert_success("No new records. Skipping download.")
      update_polis_log(
        .event = paste0(
          table_data$table,
          ": ",
          table_size,
          " new or updated records identified!"
        ),
        .event_type = "INFO"
      )
      return(NULL)
    }

    cli::cli_alert_success(paste0(
      table_data$table,
      ": ",
      table_size,
      " new or updated records identified!"
    ))
    update_polis_log(
      .event = paste0(
        table_data$table,
        ": ",
        table_size,
        " new or updated records identified!"
      ),
      .event_type = "INFO"
    )

    days_interval <- ifelse(parallel_calls, 7, NULL)
    urls <- create_table_urls(table_url, table_data, days_interval)

    cli::cli_process_start("Downloading data")
    out <- call_urls(urls)

    update_polis_log(
      .event = paste0(
        "Downloaded ",
        table_size,
        " rows of ",
        table_data$table,
        " data"
      ),
      .event_type = "INFO"
    )

    cli::cli_process_done()

    # check ids and make list of ids to be deleted
    cli::cli_process_start("Getting table Ids")
    ids <- get_table_ids(table_data, parallel_calls = FALSE)
    cli::cli_process_done()

    # Get full size of the table and the table IDs
    full_table_size <- get_table_size(table_data$table)

    # load in cache
    cli::cli_process_start("Loading existing cache")

    old_cache <- collate_file_extracts(table_data)

    cli::cli_process_done()

    # Create extract for new pull
    create_extract_file(table_data, out)

    old_cache_n <- nrow(old_cache)
    new_data_ids_in_old_cache <- sum(dplyr::pull(out[table_data$polis_id]) %in% dplyr::pull(old_cache[table_data$polis_id]))
    new_data_ids <- table_size - new_data_ids_in_old_cache
    deleted_ids <- dplyr::pull(old_cache[table_data$polis_id])[!dplyr::pull(old_cache[table_data$polis_id]) %in% ids] # ids contain all the ids available

    cli::cli_h3(paste0("'", table_data$table, "'", " table data"))
    cli::cli_bullets(c(
      "*" = paste0(table_size, " new rows of data downloaded"),
      "*" = paste0(old_cache_n, " rows of data available in old cache"),
      "*" = paste0(
        new_data_ids,
        " new ",
        table_data$polis_id,
        "s identified"
      ),
      "*" = paste0(new_data_ids_in_old_cache, " rows of data being updated"),
      "*" = paste0(length(deleted_ids), " rows of data were deleted")
    ))

    update_polis_log(
      .event = paste0(
        table_data$table,
        " - update - ",
        table_size,
        " new rows of data downloaded; ",
        old_cache_n,
        " rows of data available in old cache; ",
        new_data_ids,
        " new ",
        table_data$polis_id,
        "s identified; ",
        new_data_ids_in_old_cache,
        " rows of data being updated;",
        paste0(length(deleted_ids), " rows of data were deleted - "),
        paste0(deleted_ids, collapse = ", ")
      ),
      .event_type = "INFO"
    )

    # Update cache
    updated_cache <- bind_and_reconcile(out, old_cache |> collect())

    # Remove duplicates based on unique ID
    updated_cache <- updated_cache |>
      dplyr::arrange(dplyr::desc(get(table_data$polis_update_id))) |>
      dplyr::distinct(get(table_data$polis_id), .keep_all = TRUE)

    # Remove deleted data
    updated_cache <- updated_cache |>
      dplyr::mutate(dplyr::across(dplyr::any_of(table_data$polis_id),
                                  \(x) as.character(x))) |>
      dplyr::filter(get(table_data$polis_id) %in% ids)

    # Check for missed IDs
    cli::cli_process_start("Checking for missed records in download")
    ids_table <- as.data.frame(ids)
    missed.id <- ids_table |>
      dplyr::filter(!ids %in% dplyr::pull(old_cache[table_data$polis_id]))
    cli::cli_process_done()

    if (nrow(missed.id) != 0) {

      cli::cli_alert_info(
        paste0(
          table_data$endpoint,
          " has been downloaded before but ",
          nrow(missed.id),
          " record(s) missing, downloading data from missed record(s)..."
        )
      )
      request_missing_recs <- paste0(table_url, "?$filter=",table_data$polis_id,
                                      " in ", "('", paste0(missed.id, collapse = "','"), "')")
      request_missing_recs <- gsub(" ", "+", request_missing_recs)

      missing_epids_data <- call_single_url(request_missing_epids)

      create_extract_file(table_data, missing_epids_data)
      out <- dplyr::bind_rows(out, missing_epids_data)

    }

    updated_cache <- bind_and_reconcile(missing_epids_data, updated_cache)
    cli::cli_alert_success("Added missing records to the cache")

    cli::cli_process_start("Updating cache log")
    update_polis_cache(
      cache_file = Sys.getenv("POLIS_CACHE_FILE"),
      .table = table_data$table,
      .nrow = nrow(updated_cache),
      .update_val = max(lubridate::as_datetime(dplyr::pull(out[table_data$polis_update_id])))
    )
    cli::cli_process_done()

    cli::cli_process_start("Writing data cache")
    tidypolis_io(
      obj = updated_cache, io = "write",
      file_path = paste0(
        Sys.getenv("POLIS_DATA_CACHE"),
        "/",
        table_data$table,
        ".parquet"
      )
    )

    update_polis_log(
      .event = paste0(table_data$table, " data saved locally"),
      .event_type = "PROCESS"
    )

    cli::cli_process_done()

  return(NULL)

}

#' Download the full table from POLIS
#'
#' @inheritParams update_polis_table
#'
#' @returns `NULL` upon success.
#' @keywords internal
#'
download_full_polis_table <- function(table_data, table_url, parallel_calls = TRUE) {

  table_size <- get_table_size(.table = table_data$table)
  cli::cli_alert_info(paste0("Getting ready to download ", table_size, " new rows of data!"))

  # Create table URLs here; where to shard will depend on the table
  days_interval <- ifelse(parallel_calls, 365, 0)
  urls <- create_table_urls(table_url, table_data, days_interval)

  cli::cli_process_start("Downloading data")
  out <- call_urls(urls)

  if (nrow(out) != table_size) {
    error_message <- paste0(
      "Downloaded ",
      nrow(out),
      " rows of ",
      table_data$table,
      " data. However, expected to download ",
      table_size," rows of data."
    )

    cli::cli_alert_danger(error_message)
    update_polis_log(
      .event = error_message,
      .event_type = "ERROR"
    )
  } else {
    update_polis_log(
      .event = paste0(
        "Downloaded ",
        nrow(out),
        " rows of ",
        table_data$table,
        " data"
      ),
      .event_type = "INFO"
    )
  }

  cli::cli_process_done()

  # update cache information
  cli::cli_process_start("Updating metadata cache")
  if (is.na(table_data$polis_update_id)) {
    update_polis_cache(
      cache_file = Sys.getenv("POLIS_CACHE_FILE"),
      .table = table_data$table,
      .nrow = nrow(out),
      .update_val = NA
    )
  } else {
    update_polis_cache(
      cache_file = Sys.getenv("POLIS_CACHE_FILE"),
      .table = table_data$table,
      .nrow = nrow(out),
      .update_val = max(lubridate::as_datetime(dplyr::pull(out[table_data$polis_update_id])), na.rm = T)
    )
  }

  cli::cli_process_done()

  cli::cli_process_start("Writing data cache")

  create_extract_file(table_data, out)

  # Check if the collated table exists
  collated_table_name <- paste0(Sys.getenv("POLIS_DATA_CACHE"),"/",
                                table_data$table,".parquet")
  collated_table_exists <- tidypolis_io(io = "exists.file",
                                        file_path = collated_table_name)

  if (collated_table_exists) {
    cli::cli_alert_info("Current table will be replaced in full as the full table was downloaded.")
    tidypolis_io(obj = out, io = "write", file_path = paste0(
      Sys.getenv("POLIS_DATA_CACHE"),
      "/",
      table_data$table,
      ".parquet"
    ))
  }

  update_polis_log(
    .event = paste0(table_data$table, " data saved"),
    .event_type = "PROCESS"
  )

  cli::cli_process_done()
  gc()

}

# Main function ----


#' Request data for single table in POLIS
#'
#' @description
#' Updates or pulls the full table from the API for the table specified.
#'
#' @param .table `str` Name of the table to retrieve. Valid values include cache, virus, case,
#' human_specimen, environmental_sample, activity, sub_activity, lqas, im, population, geography.
#' @param api_key `str` API Key. Defaults to the value of the global env variable POLIS_API_KEY.
#' @param parallel_calls `logical` Whether to obtain data in parallel, or sequentially. Defaults to `TRUE`.
#' @returns `NULL` upon success.
#' @examples
#' \dontrun{
#' get_table_data("case")
#' get_table_data("virus")
#' }
#' @export
get_table_data <- function(.table, api_key = Sys.getenv("POLIS_API_Key"), parallel_calls = TRUE) {

  base_url <- "https://extranet.who.int/polis/api/v2/"
  table_data <- get_polis_cache(.table = .table)
  table_url <- paste0(base_url, table_data$endpoint)

  if (api_key == "") {
    cli::cli_abort("Please run {.code init_tidypolis()} prior to pulling table data.")
  }

  # Check if extracts folder exist
  if (!tidypolis_io(io = "exists.dir",
                    file_path = file.path(Sys.getenv("POLIS_DATA_CACHE"), "raw_extracts"))) {
    # If not, create it
    tidypolis_io(io = "create",
                 file_path = file.path(Sys.getenv("POLIS_DATA_CACHE"), "raw_extracts"))
  }

  # Check if table extract folder exist
  extract_table_folder <- file.path(Sys.getenv("POLIS_DATA_CACHE"), "raw_extracts", table_data$table)

  if (!tidypolis_io(io = "exists.dir", file_path = extract_table_folder)) {
    # If not, create it
    tidypolis_io(io = "create", file_path = extract_table_folder)
  }

  # check if ID API works for key files
  api_url <- paste0( base_url, table_data$endpoint, "?$top=1&$select=",
                     table_data$polis_id)

  id_return <- tryCatch(
    call_single_url(api_url, times = 1),
    error = function(cond) {
      return("Error")
    }
  )

  if (nrow(id_return) == 1) {
    id_error <- FALSE
    cli::cli_alert_success(paste0("Unique ID column functional for ", table_data$endpoint))
  } else {
    id_error <- TRUE
    cli::cli_alert_danger(paste0("Unique ID column functional for ", table_data$endpoint, " is non-functional."))
  }

  rm(api_url)
  full_dl <- get_full_table(id_error, table_data)
  cli::cli_h1(paste0("Downloading POLIS Data for: ", table_data$endpoint))


  if (full_dl) {
    download_full_polis_table(table_data, table_url, parallel_calls)
  } else {
    update_polis_table(table_data, table_url, parallel_calls)
  }

  return(NULL)

}
