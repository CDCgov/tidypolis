#' Request data from single table
#'
#' @description Get POLIS table Data
#' @param api_key API Key
#' @param .table Table value to retrieve
#' @returns Tibble with reference data
#' @examples
#' \dontrun{
#' get_table_data(.table = "case")
#' get_table_data(.table = "virus") # must run init_tidypolis first in order to specify API key
#' }
#' @export
get_table_data <- function(api_key = Sys.getenv("POLIS_API_Key"),
                           .table) {
  base_url <- "https://extranet.who.int/polis/api/v2/"
  table_data <- get_polis_cache(.table = .table)
  table_url <- paste0(base_url, table_data$endpoint)

  # check if ID API works for key files
  api_url <-
    paste0(
      base_url,
      table_data$endpoint,
      "?$select=",
      table_data$polis_id
    )

  if (table_data$table %in% c(
    "human_specimen",
    "environmental_sample",
    "activity",
    "sub_activity",
    "lqas",
    "pop"
  )) {
    urls <-
      create_table_urls(
        url = api_url,
        table_size = 3000,
        type = "lab-partial"
      )
  } else {
    urls <-
      create_table_urls(
        url = api_url,
        table_size = 3000,
        type = "partial"
      )
  }

  id_return <- tryCatch(
    call_single_url(urls[1], times = 1),
    error = function(cond) {
      return("Error")
    }
  )

  id_error <- is.character(id_return)

  rm(urls)
  rm(api_url)

  cli::cli_h1(paste0("Downloading POLIS Data for: ", table_data$table))

  # If never downloaded before or if ID API doesn't work
  if ((is.na(table_data$last_sync) &
       !is.na(table_data$polis_id)) |
      id_error | is.na(table_data$polis_update_id)) {
    if (id_error) {
      cli::cli_alert_info(
        paste0(
          table_data$endpoint,
          " has been downloaded before but the ID API is not functional, downloading all data...checking size..."
        )
      )
    } else {
      if (is.na(table_data$polis_update_id)) {
        cli::cli_alert_info(
          paste0(
            table_data$endpoint,
            " does not have a unique timestamps for the update, the entire table must be downloaded..."
          )
        )
      } else {
        cli::cli_alert_info(
          paste0(
            table_data$endpoint,
            " has not been downloaded before...checking size..."
          )
        )
      }
    }
    table_size <- get_table_size(.table = table_data$table)
    cli::cli_alert_info(paste0("Getting ready to download ", table_size, " new rows of data!"))

    if (table_data$table %in% c(
      "human_specimen",
      "environmental_sample",
      "activity",
      "sub_activity",
      "lqas",
      "pop"
    )) {
      urls <-
        create_table_urls(
          url = table_url,
          table_size = table_size,
          type = "lab"
        )
    } else {
      urls <-
        create_table_urls(
          url = table_url,
          table_size = table_size,
          type = "full"
        )
    }

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

    # update cache information
    cli::cli_process_start("Updating cache")
    if (is.na(table_data$polis_update_id)) {
      update_polis_cache(
        cache_file = Sys.getenv("POLIS_CACHE_FILE"),
        .table = .table,
        .nrow = nrow(out),
        .update_val = NA
      )
    } else {
      update_polis_cache(
        cache_file = Sys.getenv("POLIS_CACHE_FILE"),
        .table = .table,
        .nrow = nrow(out),
        .update_val = max(lubridate::as_datetime(dplyr::pull(out[table_data$polis_update_id])), na.rm = T)
      )
    }

    cli::cli_process_done()

    cli::cli_process_start("Writing data cache")
    tidypolis_io(obj = out, io = "write", file_path = paste0(
      Sys.getenv("POLIS_DATA_CACHE"),
      "/",
      table_data$table,
      ".rds"
    ))
    update_polis_log(
      .event = paste0(table_data$table, " data saved locally"),
      .event_type = "PROCESS"
    )
    cli::cli_process_done()

    gc()
  } else {
    if (!is.na(table_data$last_sync)) {
      # pull updated data
      # create new table url

      time_modifier <- paste0(
        "&$filter=",
        table_data$polis_update_id,
        " gt ",
        sub(" ", "T", as.character(table_data$polis_update_value)), "Z")

      time_modifier <- gsub(" ", "+", time_modifier)

      table_size <-
        get_table_size(.table = table_data$table, extra_filter = time_modifier)

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

      if (table_size > 0) {
        table_url <- paste0(
          table_url,
          "?$filter=",
          table_data$polis_update_id,
          " gt ",
          sub(" ", "T", as.character(table_data$polis_update_value)),
          "Z"
        )

        table_url <- gsub(" ", "+", table_url)

        if (table_data$table %in% c(
          "human_specimen",
          "environmental_sample",
          "activity",
          "sub_activity",
          "lqas"
        )) {
          urls <-
            create_table_urls(
              url = table_url,
              table_size = table_size,
              type = "lab-partial"
            )
        } else {
          urls <-
            create_table_urls(
              url = table_url,
              table_size = table_size,
              type = "partial"
            )
        }

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
        ids <-
          get_table_ids(.table = table_data$table, .id = table_data$polis_id)
        cli::cli_process_done()

        # load in cache
        cli::cli_process_start("Loading existing cache")
        old_cache <-
          tidypolis_io(io = "read", file_path = paste0(
            Sys.getenv("POLIS_DATA_CACHE"),
            "/",
            table_data$table,
            ".rds"
          ))
        cli::cli_process_done()
        old_cache_n <- nrow(old_cache)
        new_data_ids_in_old_cache <-
          sum(dplyr::pull(out[table_data$polis_id]) %in% dplyr::pull(old_cache[table_data$polis_id]))
        new_data_ids <- table_size - new_data_ids_in_old_cache
        deleted_ids <-
          dplyr::pull(old_cache[table_data$polis_id])[!dplyr::pull(old_cache[table_data$polis_id]) %in% ids]
        # old_data_ids_in_new <- dplyr::pull(old_cache[table_data$polis_id])[dplyr::pull(old_cache[table_data$polis_id]) %in% dplyr::pull(out[table_data$polis_id])]

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

        # update cache
        old_cache <- old_cache |>
          dplyr::filter(!get(table_data$polis_id) %in% dplyr::pull(out[table_data$polis_id]))
        old_cache <-
          bind_and_reconcile(new_data = out, old_data = old_cache)

        # delete data that no longer exists in POLIS
        old_cache <- old_cache |>
          dplyr::filter(get(table_data$polis_id) %in% ids)

        # check for missed IDs, if IDs missed then redownload full table
        # create ids table in order to filter
        cli::cli_process_start("Checking for missed records in download")
        ids_table <- as.data.frame(ids)
        missed.id <- ids_table |>
          dplyr::filter(!ids %in% dplyr::pull(old_cache[table_data$polis_id]))
        cli::cli_process_done()

        # if there are missed IDs, clear old cache and re-download full table
        if (nrow(missed.id) > 0) {
          cli::cli_alert_info(
            paste0(
              table_data$endpoint,
              " has been downloaded before but ",
              nrow(missed.id),
              " record(s) missing, downloading all data...checking size..."
            )
          )

          table_size <- get_table_size(.table = table_data$table)
          cli::cli_alert_info(paste0("Getting ready to download ", table_size, " new rows of data!"))

          table_url <- paste0(base_url, table_data$endpoint)

          if (table_data$table %in% c(
            "human_specimen",
            "environmental_sample",
            "activity",
            "sub_activity",
            "lqas",
            "pop"
          )) {
            urls <-
              create_table_urls(
                url = table_url,
                table_size = table_size,
                type = "lab"
              )
          } else {
            urls <-
              create_table_urls(
                url = table_url,
                table_size = table_size,
                type = "full"
              )
          }

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

          # update cache information
          cli::cli_process_start("Updating cache")
          if (is.na(table_data$polis_update_id)) {
            update_polis_cache(
              cache_file = Sys.getenv("POLIS_CACHE_FILE"),
              .table = .table,
              .nrow = nrow(out),
              .update_val = NA
            )
          } else {
            update_polis_cache(
              cache_file = Sys.getenv("POLIS_CACHE_FILE"),
              .table = .table,
              .nrow = nrow(out),
              .update_val = max(lubridate::as_datetime(dplyr::pull(out[table_data$polis_update_id])), na.rm = T)
            )
          }

          cli::cli_process_done()

          cli::cli_process_start("Writing data cache")
          tidypolis_io(obj = out, io = "write", file_path = paste0(
            Sys.getenv("POLIS_DATA_CACHE"),
            "/",
            table_data$table,
            ".rds"
          ))
          update_polis_log(
            .event = paste0(table_data$table, " data saved locally"),
            .event_type = "PROCESS"
          )
          cli::cli_process_done()

          gc()

          cli::cli_process_done()
        } else {
          # write cache

          cli::cli_process_start("Updating cache log")
          update_polis_cache(
            cache_file = Sys.getenv("POLIS_CACHE_FILE"),
            .table = .table,
            .nrow = nrow(old_cache),
            .update_val = max(lubridate::as_datetime(dplyr::pull(out[table_data$polis_update_id])))
          )
          cli::cli_process_done()

          cli::cli_process_start("Writing data cache")
          tidypolis_io(
            obj = old_cache, io = "write",
            file_path = paste0(
              Sys.getenv("POLIS_DATA_CACHE"),
              "/",
              table_data$table,
              ".rds"
            )
          )
          update_polis_log(
            .event = paste0(table_data$table, " data saved locally"),
            .event_type = "PROCESS"
          )
          cli::cli_process_done()

          # garbage clean
          gc()
        }
      }
    }
  }
}
