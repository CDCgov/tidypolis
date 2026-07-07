#' Tidypolis lab cleaning
#'
#' Self-contained lab cleaning helpers for tidypolis.
#' These functions do not call `clean_lab_data()` or other helpers from
#'
#'
#' @keywords internal
NULL

#Private Fuctions -----

#' Normalize country filter input
#' @param ctry_name `str` or `chr` Country name(s), optionally comma-delimited.
#' @returns `chr` Uppercase vector of countries.
#' @keywords internal
normalize_lab_ctry_filter <- function(ctry_name) {
  #If no country filter was supplied, return NULL unchanged.
  if (is.null(ctry_name)) {
    return(NULL)
  }

  #Standardize case/whitespace and allow comma-delimited filters.
  stringr::str_to_upper(stringr::str_trim(ctry_name)) |>
    stringr::str_replace_all(", ", ",") |>
    stringr::str_split(",") |>
    unlist()
}

#' Minimal WHO region resolver
#' @param country_name `chr` Country names.
#' @returns `chr` Region code.
#' @keywords internal
get_region <- function(country_name) {
  #Normalize to uppercase trimmed names for stable matching.
  country_name <- stringr::str_trim(stringr::str_to_upper(country_name))
  emro_ctry <- c(
    "AFGHANISTAN", "BAHRAIN", "DJIBOUTI", "EGYPT", "IRAN (ISLAMIC REPUBLIC OF)",
    "IRAQ", "JORDAN", "KUWAIT", "LEBANON", "LIBYA", "MOROCCO",
    "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM", "OMAN", "PAKISTAN",
    "QATAR", "SAUDI ARABIA", "SOMALIA", "SUDAN", "SYRIAN ARAB REPUBLIC", "TUNISIA",
    "UNITED ARAB EMIRATES", "YEMEN"
  )

  #Minimal binary resolver: EMRO if listed, otherwise AFRO.
  dplyr::if_else(country_name %in% emro_ctry, "EMRO", "AFRO")
}

#' Normalize regional country names/codes to match sirfunctions behavior
#' @param country_name `chr`
#' @returns `chr`
#' @keywords internal
normalize_regional_country <- function(country_name) {
  #Harmonize regional 3-letter codes/aliases into canonical names.
  dplyr::case_match(
    stringr::str_to_upper(country_name),
    "AFG" ~ "AFGHANISTAN",
    "BAH" ~ "BAHRAIN",
    "DJI" ~ "DJIBOUTI",
    "EGY" ~ "EGYPT",
    "IRN" ~ "IRAN (ISLAMIC REPUBLIC OF)",
    "IRQ" ~ "IRAQ",
    "JOR" ~ "JORDAN",
    "KUW" ~ "KUWAIT",
    "LEB" ~ "LEBANON",
    "LIB" ~ "LIBYA",
    "MOR" ~ "MOROCCO",
    "OMA" ~ "OMAN",
    "PAK" ~ "PAKISTAN",
    "PNA" ~ "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
    "QAT" ~ "QATAR",
    "SAA" ~ "SAUDI ARABIA",
    "SOM" ~ "SOMALIA",
    "SUD" ~ "SUDAN",
    "SYR" ~ "SYRIAN ARAB REPUBLIC",
    "TUN" ~ "TUNISIA",
    "UAE" ~ "UNITED ARAB EMIRATES",
    "YEM" ~ "YEMEN",
    .default = stringr::str_to_upper(country_name)
  )
}

#' Adjust rolling-year end period for analysis end date
#' @param data `tibble` Output that already includes rolling-year columns.
#' @param end_date `str` Analysis end date.
#' @param date_col `str` Date column used for rolling-year assignment.
#' @returns `tibble`
#' @keywords internal
adjust_rolling_years <- function(data, end_date, date_col) {
  if (!"rolling_period" %in% names(data)) {
    cli::cli_abort("Please pass data with rolling period columns added.")
  }

  end_date <- lubridate::as_date(end_date)

  latest_period <- data |>
    dplyr::select(
      year_label, rolling_period,
      analysis_year_start, analysis_year_end
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      year_number = as.integer(stringr::str_extract(year_label, "[-+]?\\d+"))
    ) |>
    dplyr::filter(year_number == max(year_number, na.rm = TRUE))

  if (is.na(max(data[[date_col]], na.rm = TRUE))) {
    cli::cli_alert(paste0("'", date_col, "'", " is an empty vector."))
    return(data)
  }

  data |>
    dplyr::mutate(
      year_number = as.integer(stringr::str_extract(year_label, "[-+]?\\d+"))
    ) |>
    dplyr::filter(
      !!rlang::sym(date_col) <= end_date,
      year_number > 0
    ) |>
    dplyr::mutate(
      analysis_year_end = dplyr::if_else(
        year_label == latest_period$year_label,
        end_date,
        analysis_year_end
      ),
      rolling_period = dplyr::if_else(
        year_label == latest_period$year_label,
        paste0(
          lubridate::month(.data$analysis_year_start, label = TRUE, abbr = TRUE),
          " ", lubridate::year(.data$analysis_year_start),
          " - ",
          lubridate::month(.data$analysis_year_end, label = TRUE, abbr = TRUE),
          " ", lubridate::year(.data$analysis_year_end)
        ),
        rolling_period
      )
    )
}

#' Add rolling years for output
#' @param df `tibble`
#' @param start_date `str`
#' @param end_date `str`
#' @param date_col `str`
#' @returns `tibble`
#' @keywords internal
add_rolling_years <- function(df, start_date, end_date, date_col,
                              period = lubridate::years(1)) {
  #Anchor rolling-year math on normalized start_date.
  start_date <- lubridate::as_date(start_date)

  # Normalize input date column defensively to Date
  #Ensure the selected date column is truly Date-like before intervals.
  df <- df |>
    dplyr::mutate(
      !!rlang::sym(date_col) := as.Date(as.character(.data[[date_col]]))
    )

  n_rows <- nrow(df)
  n_row_col <- sum(is.na(df[[date_col]]))

  #Hard fail if the chosen date column is entirely missing.
  if (n_rows == n_row_col) {
    cli::cli_abort("The date_col selected is an NA vector. Please check your data.")
  }

  #Build interval index, year labels, and display rolling period text.
  df <- df |>
    dplyr::mutate(
      date_interval = lubridate::interval(start_date, !!rlang::sym(date_col)),
      year_num = floor(date_interval / period),
      year_label = paste0("Year ", year_num + 1),

      analysis_year_start = lubridate::`%m+%`(start_date, lubridate::years(year_num)),
      analysis_year_start = dplyr::if_else(
        lubridate::leap_year(analysis_year_start) &
          lubridate::month(analysis_year_start) == 2 &
          lubridate::day(analysis_year_start) == 28 &
          year_num != 0,
        lubridate::`%m+%`(
          lubridate::`%m+%`(start_date, lubridate::years(year_num)),
          lubridate::days(1)
        ),
        analysis_year_start
      ),

      analysis_year_end = lubridate::`%m-%`(
        lubridate::`%m+%`(analysis_year_start, period),
        lubridate::days(1)
      ),
      analysis_year_end = dplyr::if_else(
        lubridate::leap_year(analysis_year_end) &
          lubridate::month(analysis_year_end) == 2 &
          lubridate::day(analysis_year_end) == 27,
        lubridate::`%m+%`(analysis_year_end, lubridate::days(1)),
        analysis_year_end
      ),

      rolling_period = paste0(
        lubridate::month(analysis_year_start, label = TRUE, abbr = TRUE),
        " ", lubridate::year(analysis_year_start),
        " - ",
        lubridate::month(analysis_year_end, label = TRUE, abbr = TRUE),
        " ", lubridate::year(analysis_year_end)
      )
    ) |>
    dplyr::select(-"year_num")

  #Delegate any end-date capping/trimming to shared helper.
  df <- adjust_rolling_years(df, end_date, date_col)

  return(df)
}

#' Impute missing country/province/district using AFP line list
#' @param lab_data `tibble`
#' @param afp_data `tibble`
#' @returns `tibble`
#' @keywords internal
impute_missing_lab_geo <- function(lab_data, afp_data = NULL) {
  #Backfill EPID name variant used in some extracts.
  if (!"EPID" %in% names(lab_data) && "EpidNumber" %in% names(lab_data)) {
    lab_data <- dplyr::rename_with(lab_data, dplyr::recode, EpidNumber = "EPID")
  }

  #If no AFP data, add empty geo columns and return as-is.
  if (is.null(afp_data)) {
    lab_data$ctry <- NA
    lab_data$prov <- NA
    lab_data$dist <- NA
    lab_data$adm0guid <- NA
    lab_data$adm1guid <- NA
    lab_data$adm2guid <- NA
    return(lab_data)
  }

  #Normalize AFP place column names to match downstream expectations.
  afp_data <- dplyr::rename_with(afp_data, dplyr::recode,
                                 place.admin.0 = "ctry",
                                 place.admin.1 = "prov",
                                 place.admin.2 = "dist"
  )

  #If EPID is still absent in lab_data, cannot join; return empty geo cols.
  if (!"EPID" %in% names(lab_data)) {
    lab_data$ctry <- NA
    lab_data$prov <- NA
    lab_data$dist <- NA
    lab_data$adm0guid <- NA
    lab_data$adm1guid <- NA
    lab_data$adm2guid <- NA
    return(lab_data)
  }

  #Match by EPID and copy location/admin hierarchy fields across.
  match_idx <- match(as.character(lab_data$EPID), as.character(afp_data$epid))
  copy_matched_afp_col <- function(output_col, candidate_cols) {
    if (!output_col %in% names(lab_data)) {
      lab_data[[output_col]] <<- NA_character_
    }
    matched_cols <- candidate_cols[candidate_cols %in% names(afp_data)]
    if (length(matched_cols) == 0) {
      return(invisible(NULL))
    }
    source_col <- matched_cols[[1]]
    lab_data[[output_col]] <<- dplyr::coalesce(
      as.character(lab_data[[output_col]]),
      as.character(afp_data[[source_col]][match_idx])
    )
  }


  copy_matched_afp_col("ctry", c("ctry", "country", "place.admin.0"))
  copy_matched_afp_col("prov", c("prov", "province", "place.admin.1"))
  copy_matched_afp_col("dist", c("dist", "district", "place.admin.2"))
  copy_matched_afp_col("adm0guid", c("adm0guid"))
  copy_matched_afp_col("adm1guid", c("adm1guid"))
  copy_matched_afp_col("adm2guid", c("adm2guid"))
  copy_matched_afp_col("epid_ctry", c("epid_ctry", "epid.ctry"))
  copy_matched_afp_col("epid_prov", c("epid_prov", "epid.prov"))
  copy_matched_afp_col("epid_dist", c("epid_dist", "epid.dist"))
  copy_matched_afp_col("epid_04", c("epid_04", "epid.04"))
  copy_matched_afp_col("epid_05", c("epid_05", "epid.05"))

  lab_data
}

#' Populate EPID component columns
#' @param lab_data `tibble`
#' @returns `tibble`
#' @keywords internal
add_epid_components <- function(lab_data) {
  epid_component_map <- list(
    epid_ctry = c("epid_ctry", "epid.ctry"),
    epid_prov = c("epid_prov", "epid.prov"),
    epid_dist = c("epid_dist", "epid.dist"),
    epid_04 = c("epid_04", "epid.04"),
    epid_05 = c("epid_05", "epid.05")
  )
  for (col in names(epid_component_map)) {
    if (!col %in% names(lab_data)) {
      lab_data[[col]] <- NA_character_
    }
    candidate_cols <- epid_component_map[[col]]
    candidate_cols <- candidate_cols[candidate_cols %in% names(lab_data)]
    for (candidate_col in setdiff(candidate_cols, col)) {
      lab_data[[col]] <- dplyr::coalesce(
        as.character(lab_data[[col]]),
        as.character(lab_data[[candidate_col]])
      )
    }
  }

  epid_source <- dplyr::coalesce(
    if ("EpidNumber" %in% names(lab_data)) as.character(lab_data$EpidNumber) else NA_character_,
    if ("EPID" %in% names(lab_data)) as.character(lab_data$EPID) else NA_character_,
    if ("epid" %in% names(lab_data)) as.character(lab_data$epid) else NA_character_
  )
  epid_source <- stringr::str_squish(epid_source)
  epid_source[epid_source == ""] <- NA_character_

  split_epid <- strsplit(epid_source, "[^[:alnum:]]+")
  split_part <- function(idx) {
    vapply(
      split_epid,
      function(parts) {
        parts <- parts[parts != ""]
        if (length(parts) >= idx) {
          parts[[idx]]
        } else {
          NA_character_
        }
      },
      character(1)
    )
  }

  compact_epid <- stringr::str_replace_all(epid_source, "[^[:alnum:]]", "")
  parsed_cols <- list(
    epid_ctry = dplyr::coalesce(split_part(1), substr(compact_epid, 1, 3)),
    epid_prov = dplyr::coalesce(split_part(2), substr(compact_epid, 4, 6)),
    epid_dist = dplyr::coalesce(split_part(3), substr(compact_epid, 7, 9)),
    epid_04 = split_part(4),
    epid_05 = split_part(5)
  )

  for (col in names(parsed_cols)) {
    parsed_value <- parsed_cols[[col]]
    parsed_value[is.na(epid_source)] <- NA_character_
    parsed_value[parsed_value == ""] <- NA_character_
    lab_data[[col]] <- dplyr::coalesce(as.character(lab_data[[col]]), parsed_value)
  }

  lab_data
}

#' Populate EPID component columns
#' @param lab_data `tibble`
#' @returns `tibble`
#' @keywords internal
add_epid_components <- function(lab_data) {
  epid_component_cols <- c("epid_ctry", "epid_prov", "epid_dist", "epid_04", "epid_05")
  for (col in epid_component_cols) {
    if (!col %in% names(lab_data)) {
      lab_data[[col]] <- NA_character_
    }
  }

  epid_source <- dplyr::coalesce(
    if ("EpidNumber" %in% names(lab_data)) as.character(lab_data$EpidNumber) else NA_character_,
    if ("EPID" %in% names(lab_data)) as.character(lab_data$EPID) else NA_character_
  )
  epid_source <- stringr::str_squish(epid_source)
  epid_source[epid_source == ""] <- NA_character_

  split_epid <- strsplit(epid_source, "[^[:alnum:]]+")
  split_part <- function(idx) {
    vapply(
      split_epid,
      function(parts) {
        parts <- parts[parts != ""]
        if (length(parts) >= idx) {
          parts[[idx]]
        } else {
          NA_character_
        }
      },
      character(1)
    )
  }

  compact_epid <- stringr::str_replace_all(epid_source, "[^[:alnum:]]", "")
  parsed_cols <- list(
    epid_ctry = dplyr::coalesce(split_part(1), substr(compact_epid, 1, 3)),
    epid_prov = dplyr::coalesce(split_part(2), substr(compact_epid, 4, 6)),
    epid_dist = dplyr::coalesce(split_part(3), substr(compact_epid, 7, 9)),
    epid_04 = split_part(4),
    epid_05 = split_part(5)
  )

  for (col in names(parsed_cols)) {
    parsed_value <- parsed_cols[[col]]
    parsed_value[is.na(epid_source)] <- NA_character_
    parsed_value[parsed_value == ""] <- NA_character_
    lab_data[[col]] <- dplyr::coalesce(as.character(lab_data[[col]]), parsed_value)
  }

  lab_data
}

#' Load and normalize lab locations for regional cleaner
#' @param lab_locs_path `str` Path to lab locations CSV.
#' @returns `tibble`
#' @keywords internal
get_lab_locs <- function(lab_locs_path = NULL, use_edav = TRUE) {
  #Prefer EDAV default source unless explicit local path supplied.
  lab_locs <- if (is.null(lab_locs_path) && use_edav) {
    tryCatch(
      sirfunctions::edav_io("read", file_loc = "Data/lab/Routine_lab_testing_locations.csv"),
      error = \(e) {
        cli::cli_abort(c(
          "Unable to read lab locations from EDAV default path.",
          "i" = "Set `lab_locs_path` to a local CSV or retry with EDAV access."
        ))
      }
    )
  } else if (!is.null(lab_locs_path)) {
    readr::read_csv(lab_locs_path, show_col_types = FALSE)
  } else {
    cli::cli_abort("Please provide `lab_locs_path` when `use_edav = FALSE`.")
  }

  #Standardize casing and drop rows without a country key.
  lab_locs |>
    dplyr::mutate(country = stringr::str_to_upper(country)) |>
    dplyr::filter(!is.na(country)) |>
    dplyr::mutate(seq.capacity = stringr::str_to_lower(seq.capacity))
}


#' Apply shared sequencing routing updates
#' @param lab_data `tibble`
#' @returns `tibble`
#' @keywords internal
apply_lab_cleaning_updates <- function(lab_data) {
  #Ensure expected routing flag columns exist before conditional updates.
  if (!"seq.cat" %in% names(lab_data)) {
    lab_data$seq.cat <- NA_character_
  }
  if (!"seq.capacity" %in% names(lab_data)) {
    lab_data$seq.capacity <- NA_character_
  }

  #Apply date-bounded routing overrides and post-cutover sequencing flags.
  lab_data |>
    dplyr::mutate(seq.lab = dplyr::case_when(
      seq.lab == "NICD-South Africa" & DateStoolCollected <= lubridate::as_date("2025-02-01") & culture.itd.lab == "Cameroon" ~ "CDC-Atlanta",
      seq.lab == "UVRI-Uganda" & DateStoolCollected <= lubridate::as_date("2025-02-01") & culture.itd.lab == "ETHIOPIA/ KEMRI-Kenya" ~ "CDC-Atlanta",
      seq.lab == "Ibadan-Nigeria" & DateStoolCollected <= lubridate::as_date("2025-02-01") & culture.itd.lab %in% c("Ibadan-Nigeria, Maiduguri-Nigeria", "Nigeria") ~ "CDC-Atlanta",
      seq.lab == "UVRI-Uganda" & DateStoolCollected <= lubridate::as_date("2025-02-01") & culture.itd.lab == "KEMRI-Kenya" ~ "CDC-Atlanta",
      seq.lab == "UVRI-Uganda" & country == "UGANDA" & DateStoolCollected <= lubridate::as_date("2025-02-01") ~ "CDC-Atlanta",
      seq.lab == "NICD-South Africa" & DateStoolCollected <= lubridate::as_date("2025-02-01") & culture.itd.lab == "Senegal" ~ "CDC-Atlanta",
      seq.lab == "Varied (UVRI/ Oman/ Jordan)" & DateStoolCollected <= lubridate::as_date("2025-02-01") & culture.itd.lab == "Varied (KEMRI-Kenya/ Oman/ Jordan)" ~ "CDC-Atlanta",
      .default = seq.lab
    )) |>
    dplyr::mutate(seq.cat = dplyr::case_when(
      DateStoolCollected >= lubridate::as_date("2025-02-01") &
        culture.itd.lab %in% c("Ibadan-Nigeria, Maiduguri-Nigeria", "Nigeria") &
        seq.lab == "Ibadan-Nigeria" ~ "Not shipped for sequencing",
      country == "UGANDA" & DateStoolCollected >= lubridate::as_date("2025-02-01") ~ "Not shipped for sequencing",
      .default = seq.cat
    )) |>
    dplyr::mutate(seq.capacity = dplyr::if_else(
      country %in% c("NIGERIA", "UGANDA") & DateStoolCollected >= lubridate::as_date("2025-02-01"),
      "Sequencing capacity", seq.capacity
    ))
}


#' Standalone WHO cleaner
#' @param lab_data `tibble`
#' @param start_date `str`
#' @param end_date `str`
#' @param afp_data `tibble`
#' @param ctry_name `str` or `chr`
#' @returns `tibble`
#' @keywords internal
clean_lab_data_who <- function(lab_data, start_date, end_date,
                               afp_data = NULL, ctry_name = NULL) {
  #Normalize analysis window inputs.
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  #Parse all Date* columns defensively across common date formats.
  lab_data <- lab_data |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("Date"),
      \(x) as.Date.character(x, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y"), optional = TRUE)
    ))

  #Derive year if missing to support year-based filtering.
  if (!"year" %in% names(lab_data)) {
    lab_data <- lab_data |>
      dplyr::mutate(year = lubridate::year(DateOfOnset))
  }

  #Compute turnaround metrics then filter to case records in range.
  out <- lab_data |>
    dplyr::mutate(
      days.collect.lab = DateStoolReceivedinLab - DateStoolCollected,
      days.lab.culture = DateFinalCellCultureResults - DateStoolReceivedinLab,
      days.seq.ship = DateIsolateRcvdForSeq - DateFinalCellCultureResults,
      days.lab.seq = DateSeqResult - DateStoolReceivedinLab,
      days.itd.seqres = DateSeqResult - DateFinalrRTPCRResults,
      days.itd.arriveseq = DateIsolateRcvdForSeq - DateFinalrRTPCRResults,
      days.seq.rec.res = DateSeqResult - DateIsolateRcvdForSeq
    ) |>
    dplyr::filter(
      dplyr::between(year, lubridate::year(start_date), lubridate::year(end_date))
    )

  #Fill geography from AFP linkage and derive WHO region.
  out <- impute_missing_lab_geo(out, afp_data)
  out <- out |>
    dplyr::mutate(whoregion = get_region(ctry))

  #Optional country filter (supports comma-delimited text input).
  if (!is.null(ctry_name)) {
    ctry_name <- normalize_lab_ctry_filter(ctry_name)
    out <- out |>
      dplyr::filter(ctry %in% ctry_name | is.na(ctry))
  }

  #Add additional field-to-lab transport timing metrics.
  out |>
    dplyr::mutate(
      days.coll.sent.field = as.numeric(DateStoolSentfromField - DateStoolCollected),
      days.sent.field.rec.nat = as.numeric(DateStoolReceivedNatLevel - DateStoolSentfromField),
      days.rec.nat.sent.lab = as.numeric(DateStoolSentToLab - DateStoolReceivedNatLevel),
      days.sent.lab.rec.lab = as.numeric(DateStoolReceivedinLab - DateStoolSentToLab),
      days.rec.lab.culture = as.numeric(DateFinalCellCultureResults - DateStoolReceivedinLab)
    )
}

#' Standalone regional cleaner
#' @param lab_data `tibble`
#' @param start_date `str`
#' @param end_date `str`
#' @param afp_data `tibble`
#' @param ctry_name `str` or `chr`
#' @param lab_locs_path `str`
#' @returns `tibble`
#' @keywords internal
clean_lab_data_regional <- function(lab_data,
                                    start_date, end_date,
                                    afp_data = NULL,
                                    ctry_name = NULL,
                                    lab_locs_path = NULL,
                                    use_edav = TRUE) {
  # Static vars
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  if (is.na(start_date) | is.na(end_date)) {
    cli::cli_abort("start_date and end_date must be in YYYY-MM-DD format.")
    return(lab_data)
  }

  lab_locs <- get_lab_locs(lab_locs_path, use_edav)

  lab_data <- dplyr::rename_with(lab_data, recode,
                                 Name = "country"
  )

  lab_data <- lab_data |>
    dplyr::mutate(country = dplyr::case_match(
      country,
      "AFG" ~ "AFGHANISTAN",
      "BAH" ~ "BAHRAIN",
      "DJI" ~ "DJIBOUTI",
      "EGY" ~ "EGYPT",
      "IRN" ~ "IRAN (ISLAMIC REPUBLIC OF)",
      "IRQ" ~ "IRAQ",
      "JOR" ~ "JORDAN",
      "KUW" ~ "KUWAIT",
      "LEB" ~ "LEBANON",
      "LIB" ~ "LIBYA",
      "MOR" ~ "MOROCCO",
      "OMA" ~ "OMAN",
      "PAK" ~ "PAKISTAN",
      "PNA" ~ "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM",
      "QAT" ~ "QATAR",
      "SAA" ~ "SAUDI ARABIA",
      "SOM" ~ "SOMALIA",
      "SUD" ~ "SUDAN",
      "SYR" ~ "SYRIAN ARAB REPUBLIC",
      "TUN" ~ "TUNISIA",
      "UAE" ~ "UNITED ARAB EMIRATES",
      "YEM" ~ "YEMEN",
      .default = country
    ))

  cli::cli_process_start("Converting date character columns to date types.")
  lab_data <- lab_data |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c(
        "CaseDate",
        "ParalysisOnsetDate",
        "DateStoolCollected",
        "StoolDateSentToLab",
        "DateStoolReceivedinLab",
        "DateFinalCellCultureResult",
        "DateFinalrRTPCRResults",
        "ReportDateSequenceResultSent",
        "DateIsolateRcvdForSeq",
        "DateLArmIsolate",
        "DateRArmIsolate",
        "DateofSequencing",
        "DateNotificationtoHQ"
      )), \(x) as.Date.character(x,
                                 tryFormats = c(
                                   "%Y-%m-%d",
                                   "%Y/%m/%d",
                                   "%m/%d/%Y"
                                 ),
                                 optional = T
      ))
    )
  cli::cli_process_done()

  cli::cli_process_start("Filtering to date range specified")
  lab_data <- lab_data |>
    dplyr::filter(dplyr::between(ParalysisOnsetDate, start_date, end_date))
  cli::cli_process_done()

  cli::cli_process_start("Deduplicating data")
  lab_data2 <- lab_data |>
    dplyr::distinct()

  # Additional cleaning steps
  # need data dictionary, in order to standardize names
  lab_data3 <- lab_data2 |>
    # Dropping rows with Specimen number 0 or >2
    dplyr::filter(SpecimenNumber %in% c(1, 2)) |>
    dplyr::mutate(
      country = stringr::str_to_upper(country),
      country = ifelse(stringr::str_detect(country, "IVOIRE"),
                       "COTE D IVOIRE", country
      ),
      year = lubridate::year(ParalysisOnsetDate),
      whoregion = get_region(country)
    )

  lab_data4 <- lab_data3 |>
    dplyr::left_join(
      lab_locs |> dplyr::select("country":"num.ship.seq.samples")
    ) |>
    dplyr::group_by(EPID, SpecimenNumber) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup()

  lab_data4 <- lab_data4 |>
    dplyr::mutate(culture.itd.lab = dplyr::case_when(
      country == "NIGERIA" ~ "Nigeria",
      culture.itd.lab == "NOGUCHI- Ghana" ~ "NOGUCHI-Ghana",
      .default = culture.itd.lab
    ))

  # Seperate blank epids from rest of lab_data4 in order to de-dupe
  # based on epid and specimen number, join back after dedup
  blank_epid <- lab_data4 |>
    dplyr::filter(is.na(EPID))

  lab_data4 <- lab_data4 |>
    dplyr::filter(!is.na(EPID)) |>
    dplyr::select(-"n")

  lab_data4 <- lab_data4[!duplicated(lab_data4[c("EPID", "SpecimenNumber")]), ]
  cli::cli_process_done()

  # Create intervals (currently using subset of those I need for SC PPT)
  cli::cli_process_start("Creating timeliness interval columns")
  lab_data5 <- lab_data4 |>
    dplyr::mutate(
      # Intervals from stool arrival to sequencing ----
      ## timeliness of stool collection to arrival in lab ----
      days.collect.lab = DateStoolReceivedinLab - DateStoolCollected,

      ## timeliness of stool arriving in lab to final culture results ----
      days.lab.culture = DateFinalCellCultureResult - DateStoolReceivedinLab,

      ## timeliness of final culture results to arrival at the sequencing lab ----
      days.seq.ship = DateIsolateRcvdForSeq - DateFinalCellCultureResult,

      ## timeliness of arrival at sequencing lab to sequencing results ----
      days.seq.rec.res = DateofSequencing - DateIsolateRcvdForSeq,,

      # Interval measuring sequencing results from date of arrival (NOT part of KPI) ----
      ## timeliness of ITD results to arrival at sequencing lab ----
      days.itd.arriveseq = DateIsolateRcvdForSeq - DateFinalrRTPCRResults,

      ## timeliness of ITD results to sequencing results ----
      days.itd.seqres = DateofSequencing - DateFinalrRTPCRResults,

      # Measures overall lab timeliness ----
      ## timeliness of arriving in lab to sequencing ----
      days.lab.seq = DateofSequencing - DateStoolReceivedinLab,

      # Met target yes/no
      met.targ.collect.lab = ifelse(days.collect.lab < 3, 1, 0),
      negative.spec = ifelse(!str_detect(FinalCellCultureResult, "ITD") &
                               FinalITDResult == "NULL", 1, 0),
      met.lab.culture = ifelse(days.lab.culture < 14, 1, 0),
    )
  cli::cli_process_done()

  cli::cli_process_start("Filtering out negative time intervals")
  lab_data5 <- lab_data5 |>
    # filtering out negative time intervals
    dplyr::filter((days.collect.lab >= 0 | is.na(days.collect.lab)) &
                    (days.lab.culture >= 0 | is.na(days.lab.culture)) &
                    (days.seq.ship >= 0 | is.na(days.seq.ship)) &
                    (days.lab.seq >= 0 | is.na(days.lab.seq)) &
                    (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
                    (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
                    (days.seq.rec.res >= 0 | is.na(days.seq.rec.res)))
  cli::cli_process_done()

  cli::cli_process_start("Filtering nonsensical dates")
  lab_data5 <- lab_data5 |>
    dplyr::filter(
      (DateStoolCollected >= ParalysisOnsetDate | is.na(ParalysisOnsetDate)),
      # (lubridate::year(DateFinalCellCultureResult) <= 2023 | is.na(DateFinalCellCultureResult)),
      # remove a blank specimen row
      !is.na(EPID)
    ) |>
    dplyr::mutate(
      seq.capacity = ifelse(seq.capacity == "yes",
                            "Sequencing capacity",
                            "No sequencing capacity"
      ),
      culture.itd.lab = ifelse(country == "NIGERIA",
                               "Nigeria", culture.itd.lab
      ),
    ) |>
    dplyr::select(-dplyr::contains("cIntratypeIs"))
  cli::cli_process_done()

  lab_data <- lab_data5
  rm(lab_data2, lab_data3, lab_data4, lab_data5)

  cli::cli_process_start("Correcting district and province names.")
  lab_data <- impute_missing_lab_geo(lab_data, afp_data)
  cli::cli_process_done()

  # Filter to only the country of interest
  if (!is.null(ctry_name)) {
    ctry_name <- stringr::str_trim(stringr::str_to_upper(ctry_name))
    # Handle when passing just one string comma delimited string
    ctry_name <- stringr::str_replace_all(ctry_name, ", ", ",") |>
      stringr::str_split(",") |>
      unlist()
    # Recode for COTE D'IVOIRE
    ctry_name <- dplyr::if_else(stringr::str_detect(ctry_name, "(?i)IVOIRE"),
                                "COTE D'IVOIRE", ctry_name
    )
    cli::cli_process_start("Filtering country-specific lab data")
    cli::cli_alert_warning(paste0(
      "NOTE: Filtering will include rows where ctry is",
      " N/A. Please review the dataset carefully after cleaning."
    ))
    lab_data <- lab_data |>
      dplyr::filter(ctry %in% ctry_name | is.na(ctry))
    cli::cli_process_done()
  }

  # adding additional subintervals (these aren't present in regional lab data, so are created as dummy variables)
  lab_data <- lab_data |>
    mutate(
      days.coll.sent.field = NA,
      days.sent.field.rec.nat = NA,
      days.rec.nat.sent.lab = NA,
      days.sent.lab.rec.lab = NA,
      days.rec.lab.culture = NA,
    )

  return(lab_data)
}

# Public Functions ----

#' Standalone top-level lab cleaner
#' @param lab_data `tibble`
#' @param start_date `str`
#' @param end_date `str`
#' @param afp_data `tibble`
#' @param ctry_name `str` or `chr`
#' @param lab_locs_path `str`
#' @returns `tibble`
#' @export
clean_lab_data <- function(lab_data, start_date, end_date,
                           afp_data = NULL, ctry_name = NULL,
                           lab_locs_path = NULL,
                           use_edav = TRUE,
                           save_rda_path = NULL) {
  #Dispatch to WHO cleaner when MasterKey schema is present,
  #otherwise use regional cleaner; then add rolling-year labels.
  input_has_master_key <- "MasterKey" %in% names(lab_data)

  lab_data <- if (input_has_master_key) {
    clean_lab_data_who(lab_data, start_date, end_date, afp_data, ctry_name) |>
      add_rolling_years(start_date, end_date, "DateOfOnset")
  } else {
    clean_lab_data_regional(
      lab_data, start_date, end_date,
      afp_data, ctry_name, lab_locs_path, use_edav
    ) |>
      add_rolling_years(start_date, end_date, "CaseDate")
  }

  #Apply shared sequencing-routing updates used by both pipelines.
  lab_data <- apply_lab_cleaning_updates(lab_data)

  if (!input_has_master_key) {
    #Regional harmonization block: normalize key IDs/country/region and add
    #WHO/sirfunctions-compatible aliases while preserving the derived fields
    #created above (turnaround metrics, rolling-year labels, and routing cols).
    if (!"country" %in% names(lab_data)) {
      lab_data$country <- dplyr::coalesce(
        if ("ctry" %in% names(lab_data)) lab_data$ctry else NA_character_,
        if ("Name" %in% names(lab_data)) lab_data$Name else NA_character_
      )
    } else {
      lab_data$country <- dplyr::coalesce(
        lab_data$country,
        if ("ctry" %in% names(lab_data)) lab_data$ctry else NA_character_,
        if ("Name" %in% names(lab_data)) lab_data$Name else NA_character_
      )
    }
    lab_data$country <- stringr::str_to_upper(lab_data$country)

    if (!"Name" %in% names(lab_data) && "country" %in% names(lab_data)) {
      lab_data$Name <- lab_data$country
    }
    if (!"ctry" %in% names(lab_data) && "country" %in% names(lab_data)) {
      lab_data$ctry <- lab_data$country
    }
    if ("region" %in% names(lab_data)) {
      lab_data$region <- dplyr::coalesce(
        lab_data$region,
        if ("whoregion" %in% names(lab_data)) lab_data$whoregion else NA_character_,
        if ("country" %in% names(lab_data)) get_region(lab_data$country) else NA_character_,
        if ("ctry" %in% names(lab_data)) get_region(lab_data$ctry) else NA_character_,
        if ("Name" %in% names(lab_data)) get_region(lab_data$Name) else NA_character_
      )
    } else {
      lab_data$region <- dplyr::coalesce(
        if ("whoregion" %in% names(lab_data)) lab_data$whoregion else NA_character_,
        if ("country" %in% names(lab_data)) get_region(lab_data$country) else NA_character_,
        if ("ctry" %in% names(lab_data)) get_region(lab_data$ctry) else NA_character_,
        if ("Name" %in% names(lab_data)) get_region(lab_data$Name) else NA_character_
      )
    }

    #Add WHO-schema aliases for regional extracts so downstream users can rely
    #on the same names that sirfunctions::clean_lab_data() exposes.
    alias_pairs <- c(
      DateOfOnset = "ParalysisOnsetDate",
      DateFinalCellCultureResults = "DateFinalCellCultureResult",
      DateSeqResult = "DateofSequencing",
      DateStoolSentToLab = "StoolDateSentToLab"
    )
    for (alias in names(alias_pairs)) {
      source_col <- unname(alias_pairs[[alias]])
      if (!alias %in% names(lab_data) && source_col %in% names(lab_data)) {
        lab_data[[alias]] <- lab_data[[source_col]]
      }
    }

    missing_cols <- setdiff(
      c(
        "EPID", "SpecimenNumber", "CaseContactCode", "Name", "CaseDate",
        "ParalysisOnsetDate", "DateOfOnset", "DateStoolCollected",
        "StoolDateSentToLab", "DateStoolSentToLab", "DateStoolReceivedinLab",
        "FinalCellCultureResult", "DateFinalCellCultureResult",
        "DateFinalCellCultureResults", "FinalITDResult",
        "DateFinalrRTPCRResults", "DateIsolateSentforSequencing",
        "ReportDateSequenceResultSent", "DateIsolateRcvdForSeq", "DateLArmIsolate",
        "DateRArmIsolate", "ResultNPENT", "cIntratypeIsV1", "cIntratypeIsV2",
        "cIntratypeIsV3", "cIntratypeIsVDPV1", "cIntratypeIsVDPV2", "cIntratypeIsVDPV3",
        "cIntratypeIsW1", "cIntratypeIsW2", "cIntratypeIsW3", "DateofSequencing",
        "DateSeqResult", "DateNotificationtoHQ", "WILD1", "VDPV1", "VDPV2",
        "VDPV3", "region", "whoregion", "ctry", "prov", "dist", "adm0guid",
        "adm1guid", "adm2guid"
      ),
      names(lab_data)
    )
    for (col in missing_cols) {
      lab_data[[col]] <- NA
    }

    lab_data <- lab_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c(
            "CaseDate", "ParalysisOnsetDate", "DateOfOnset", "DateStoolCollected",
            "StoolDateSentToLab", "DateStoolSentToLab", "DateStoolReceivedinLab",
            "DateFinalCellCultureResult", "DateFinalCellCultureResults",
            "DateFinalrRTPCRResults", "ReportDateSequenceResultSent",
            "DateIsolateRcvdForSeq", "DateLArmIsolate", "DateRArmIsolate",
            "DateofSequencing", "DateSeqResult"
          )),
          ~as.POSIXct(.x, tz = "UTC")
        ),
        DateNotificationtoHQ = suppressWarnings(as.logical(DateNotificationtoHQ)),
        DateIsolateSentforSequencing = suppressWarnings(as.logical(DateIsolateSentforSequencing)),
        CaseContactCode = suppressWarnings(as.logical(CaseContactCode)),
        ResultNPENT = suppressWarnings(as.numeric(ResultNPENT)),
        dplyr::across(
          dplyr::any_of(c("WILD1", "VDPV1", "VDPV2", "VDPV3")),
          ~{
            val <- stringr::str_to_upper(stringr::str_trim(as.character(.x)))
            dplyr::if_else(!is.na(.x) & val != "" & !val %in% c("0", "FALSE", "F", "NO", "N", "NA", "N/A", "NULL", ".", "-"), TRUE, NA)
          }
        )

    ) |>
  dplyr::relocate(
    dplyr::any_of(c(
      "EPID", "SpecimenNumber", "CaseContactCode", "country", "Name", "ctry",
      "prov", "dist", "whoregion", "region", "CaseDate", "ParalysisOnsetDate",
      "DateOfOnset", "DateStoolCollected", "StoolDateSentToLab",
      "DateStoolSentToLab", "DateStoolReceivedinLab",
      "FinalCellCultureResult", "DateFinalCellCultureResult",
      "DateFinalCellCultureResults", "FinalITDResult", "DateFinalrRTPCRResults",
      "DateIsolateSentforSequencing", "ReportDateSequenceResultSent",
      "DateIsolateRcvdForSeq", "DateLArmIsolate", "DateRArmIsolate",
      "DateofSequencing", "DateSeqResult", "ResultNPENT", "WILD1", "VDPV1",
      "VDPV2", "VDPV3", "days.collect.lab", "days.lab.culture",
      "days.seq.ship", "days.lab.seq", "days.itd.seqres", "days.itd.arriveseq",
      "days.seq.rec.res", "days.coll.sent.field", "days.sent.field.rec.nat",
      "days.rec.nat.sent.lab", "days.sent.lab.rec.lab", "days.rec.lab.culture",
      "year_label", "rolling_period", "analysis_year_start", "analysis_year_end",
      "seq.capacity", "seq.lab", "culture.itd.lab", "seq.cat"
    ))
  )

  }

  #Match the sirfunctions lab output schema so WHO and regional tidypolis
  #outputs expose the same column names and order.
  if (!"EpidNumber" %in% names(lab_data) && "EPID" %in% names(lab_data)) {
    lab_data$EpidNumber <- lab_data$EPID
  }
  if (!"CaseContactCode" %in% names(lab_data) && "CaseOrContact" %in% names(lab_data)) {
    lab_data$CaseContactCode <- lab_data$CaseOrContact
  }
  if (!"ParalysisOnsetDate" %in% names(lab_data) && "DateOfOnset" %in% names(lab_data)) {
    lab_data$ParalysisOnsetDate <- lab_data$DateOfOnset
  }
  if (!"StoolDateSentToLab" %in% names(lab_data) && "DateStoolSentToLab" %in% names(lab_data)) {
    lab_data$StoolDateSentToLab <- lab_data$DateStoolSentToLab
  }
  if (!"DateFinalCellCultureResult" %in% names(lab_data) && "DateFinalCellCultureResults" %in% names(lab_data)) {
    lab_data$DateFinalCellCultureResult <- lab_data$DateFinalCellCultureResults
  }
  if (!"DateofSequencing" %in% names(lab_data) && "DateSeqResult" %in% names(lab_data)) {
    lab_data$DateofSequencing <- lab_data$DateSeqResult
  }
  if (!"who.region" %in% names(lab_data) && "whoregion" %in% names(lab_data)) {
    lab_data$who.region <- lab_data$whoregion
  }

  lab_output_cols <- c(
    "epid_ctry", "epid_prov", "epid_dist", "epid_04", "epid_05",
    "EpidNumber", "SpecimenNumber", "CaseContactCode", "country", "CaseDate",
    "ParalysisOnsetDate", "DateStoolCollected", "StoolDateSentToLab",
    "DateStoolReceivedinLab", "FinalCellCultureResult", "DateFinalCellCultureResult",
    "FinalITDResult", "DateFinalrRTPCRResults", "DateIsolateSentforSequencing",
    "ReportDateSequenceResultSent", "DateIsolateRcvdForSeq", "DateLArmIsolate",
    "DateRArmIsolate", "ResultNPENT", "DateofSequencing", "DateNotificationtoHQ",
    "WILD1", "VDPV1", "VDPV2", "VDPV3", "region", "year", "whoregion",
    "who.region", "culture.itd.lab", "seq.lab", "wgs.lab*", "culture.itd.cat",
    "seq.capacity", "seq.cat", "num.ship.seq.samples", "days.collect.lab",
    "days.lab.culture", "days.seq.ship", "days.seq.rec.res", "days.itd.arriveseq",
    "days.itd.seqres", "days.lab.seq", "met.targ.collect.lab", "negative.spec",
    "met.lab.culture", "ctry", "adm0guid", "prov", "adm1guid", "dist", "adm2guid",
    "days.coll.sent.field", "days.sent.field.rec.nat", "days.rec.nat.sent.lab",
    "days.sent.lab.rec.lab", "days.rec.lab.culture", "date_interval", "year_label",
    "analysis_year_start", "analysis_year_end", "rolling_period", "year_number"
  )

  missing_cols <- setdiff(lab_output_cols, names(lab_data))
  for (col in missing_cols) {
    lab_data[[col]] <- NA
  }
  lab_data <- lab_data |>
    dplyr::select(dplyr::all_of(lab_output_cols))

  # .rda output
  #Optional side-effect to save cleaned output object.
  if (!is.null(save_rda_path)) {
    lab_clean_all <- out
    save(lab_clean_all, file = save_rda_path)
  }

  lab_data
}

#' Function to load the raw lab data locally
#'
#' This a function to load lab data that are either CSVs or Excel files.
#'
#' @param lab_data_path `str` File path as a string to the lab data.
#' @param sheet_name `str` Name of the sheet to load. This is optional in cases
#' of an Excel sheet with multiple tabs.
#'
#' @returns `tibble` Lab data loaded from the CSV or Excel file path.
#' @examples
#' \dontrun{
#' lab_data_path <- "C:/Users/ABC1/Desktop/lab_data.csv"
#' lab_data <- load_lab_data(lab_data_path)
#' }
#'
#' @export
load_lab_data <- function(lab_data_path, sheet_name = NULL) {
  #Guard dependency so CSV-only users get a clear error for xlsx paths.
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop('Package "readxl" must be installed to use this function.',
         .call = FALSE
    )
  }

  #Extension-based dispatch between CSV and Excel readers.
  if (stringr::str_ends(lab_data_path, ".csv")) {
    return(readr::read_csv(lab_data_path))
  } else if (stringr::str_ends(lab_data_path, ".xlsx")) {
    return(readxl::read_excel(lab_data_path, sheet = sheet_name))
  } else {
    stop("Not a csv or .xlsx file. Please try again.")
  }
}
