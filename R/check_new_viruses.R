#' Analyze errors in the Positives dataset pulled directly from the POLIS API
#'
#' @description
#' Checks for errors in the virus table downloaded from the POLIS API. It is meant
#' to catch errors related to inconsistencies in detections reported in the current
#' week.
#'
#' @details
#' NOTE: this function analyzes the virus table from the direct API pull or the positives file
#' after completing preprocessing. The virus table has missingness in its notification to HQ date
#' or the VDPV classification date, which is "patched" during preprocessing.
#' The patch involves pulling these fields directly from the AFP linelist or the ES table,
#' depending on whether the virus in question is an ES
#' sample or not, as these tables have more complete information on those two date fields. However,
#' there are instances where it is not possible to patch. FOr example, if an AFP VDPV detection in the virus
#' table is also missing its VDPV classification date in the AFP linelist.
#'
#' @param virus_table `str` or `tibble` Absolute path to the virus table, the direct virus dataset from the API pull,
#' or the positives dataset after being cleaned during preprocessing.
#' @param week_floor_date `date` Start date of the week to analyze. Defaults to the Monday of the previous week.
#' @param edav `logical` Whether to load the positives file from EDAV or locally. Defaults to TRUE.
#'
#' @returns `list` A list containing details of the data errors, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' data_errors <- check_new_viruses()
#' }
check_new_viruses <- function(virus_table =  "GID/PEB/SIR/POLIS/data/virus.rds",
                              week_floor_date = lubridate::floor_date(Sys.Date(), unit = "week", week_start = 1),
                              edav = TRUE) {
  week_floor_date <- lubridate::as_date(week_floor_date)

  if (is.character(virus_table)) {
    cli::cli_process_start("Loading positives dataset based on path provided")
    positives <- sirfunctions::sirfunctions_io("read", NULL, file_loc = virus_table_path, edav = edav)
    positives <- format_api_virus_table(positives)
    table_type <- "raw virus table"
    cli::cli_process_done()
  } else if (dplyr::is.tbl(virus_table)) {
    if ("EPID" %in% names(virus_table)) {
      cli::cli_alert_info("Looks like the raw API virus table...")
      positives <- format_api_virus_table(virus_table)
      table_type <- "raw virus table"
    } else {
      cli::cli_alert_info("Looks like the preprocessed virus table...")
      positives <- virus_table |>
        dplyr::select(dplyr::any_of(c("epid", "dateonset", "place.admin.0",
                                      "place.admin.1", "measurement",
                                      "datenotificationtohq", "vdpvclassificationchangedate",
                                      "report_date")))
      table_type <- "preprocessed virus table"
    }
  }

  # Viruses

  if (table_type == "raw virus table") {
    if ("publishdate" %in% names(positives)) {
      newly_added <- filter_current_week(positives, "publishdate", week_floor_date)
    } else {
      newly_added <- tibble()
    }

    if ("updateddate" %in% names(positives)) {
      newly_updated <- filter_current_week(positives, "updateddate", week_floor_date)
    } else {
      newly_updated <- tibble()
    }
  } else {
    newly_added <- tibble()
    newly_updated <- tibble()
    cli::cli_alert_info("At this time, the preprocessed positives dataset does not contain the created and updated date")
  }

  newly_changed_classification <- filter_current_week(positives, "vdpvclassificationchangedate", week_floor_date)


  if ("report_date" %in% names(positives)) {
    newly_reported <- filter_current_week(positives, "report_date", week_floor_date)
    new_w_good_report_date <- filter_current_week(newly_reported, "report_date", week_floor_date) |> dplyr::pull(epid)
  } else {
    newly_reported <- tibble()
    new_w_good_report_date <- character(0)
  }


  cli::cli_alert(paste0("Since ", week_floor_date, ":"))
  cli::cli_li(paste0("Newly added virus records: ", nrow(newly_added)))
  cli::cli_li(paste0("Newly updated virus records: ",  nrow(newly_updated)))
  cli::cli_li(paste0("Newly reported virus records: ", nrow(newly_reported)))

  # Records check

  if ("datenotificationtohq" %in% names(newly_added)) {
    new_w_hq_date <- filter_current_week(newly_added, "datenotificationtohq", week_floor_date) |> dplyr::pull(epid)
  } else {
    new_w_hq_date <- character(0)
  }

  if ("vdpvclassificationchangedate" %in% names(newly_added)) {
    new_w_vdpv_change_date <- filter_current_week(newly_added, "vdpvclassificationchangedate", week_floor_date) |> dplyr::pull(epid)
  } else {
    new_w_vdpv_change_date <- character(0)
  }

  # Positives records with errors
  cli::cli_process_start("Analyzing potential errors for newly added viruses")

  if (nrow(newly_added) > 0 &&
      all(c("epid", "measurement") %in% names(newly_added))) {
    new_no_hq_date <- newly_added |>
      dplyr::filter(!epid %in% new_w_hq_date, measurement == "WILD 1")
    new_no_vdpv_change_date <- newly_added |>
      dplyr::filter(!epid %in% c(new_w_vdpv_change_date),
                    measurement != "WILD 1")
    new_no_report_date <- newly_added |>
      dplyr::filter(!epid %in% c(new_w_vdpv_change_date, new_w_hq_date))
  } else {
    new_no_hq_date <- dplyr::tibble()
    new_no_vdpv_change_date <- dplyr::tibble()
    new_no_report_date <- dplyr::tibble()
  }

  cli::cli_process_done()

  # Weekly reporting error
  error_list <- list()
  if (table_type == "raw virus table") {
    if ("virusreportingweekandyear" %in% names(newly_reported)) {
      current_week <- names(which.max(table(newly_reported$virusreportingweekandyear)))
      if (is.null(current_week)) {
        cli::cli_alert_warning("No new viruses for the week selected")
      } else {
        cli::cli_alert_info(paste0("The current week is: ", current_week))
      }
    } else {
      current_week <- NULL
    }

    if ("virusreportingweekandyear" %in% names(newly_added) && nrow(newly_added) > 0 && !is.null(current_week)) {
      new_but_different_week <- newly_added |>
        dplyr::filter(virusreportingweekandyear != current_week)
    } else {
      new_but_different_week <- tibble()
    }

    error_list$new_but_different_week <- new_but_different_week
    cli::cli_li(paste0("Newly added viruses reported for a different week: ", nrow(new_but_different_week)))
  }

  cli::cli_li(paste0("Newly added WILD 1 viruses with no date notification to HQ: ", nrow(new_no_hq_date)))
  cli::cli_li(paste0("Newly added VDPV viruses with no VDPV Classification Change Date: ", nrow(new_no_vdpv_change_date)))
  cli::cli_li(paste0("Newly added viruses with no report date: ", nrow(new_no_report_date)))
  cli::cli_alert_info("NOTE: Report date is based on the following: VDPVs (VDPV Classification Change Date); WILD 1 (Notification to HQ Date)")

  cli::cli_alert_info("Please check the outputted list for more details on the issues highlighted")

  error_list$newly_added <- newly_added
  error_list$newly_updated <- newly_updated
  error_list$newly_changed_classification <- newly_changed_classification
  cli::cli_alert_info("NOTE: Viruses in newly_reported are those with report dates only.")
  error_list$newly_reported <- newly_reported
  error_list$new_no_hq_date <- new_no_hq_date
  error_list$new_no_vdpv_change_date <- new_no_vdpv_change_date
  error_list$new_no_report_date <- new_no_report_date

  invisible(error_list)

}
