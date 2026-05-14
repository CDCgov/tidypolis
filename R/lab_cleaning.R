#' Tidypolis lab cleaning
#'
#' Self-contained lab cleaning helpers intended to be copied into tidypolis.
#' These functions do not call `clean_lab_data()` or other helpers from
#' `R/dr.lab.functions.R`.
#'
#' @keywords internal
NULL

#Private Fuctions -----

#' Normalize country filter input
#' @param ctry_name `str` or `chr` Country name(s), optionally comma-delimited.
#' @returns `chr` Uppercase vector of countries.
#' @keywords internal
normalize_lab_ctry_filter <- function(ctry_name) {
  if (is.null(ctry_name)) {
    return(NULL)
  }

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
  country_name <- stringr::str_trim(stringr::str_to_upper(country_name))
  emro_ctry <- c(
    "AFGHANISTAN", "BAHRAIN", "DJIBOUTI", "EGYPT", "IRAN (ISLAMIC REPUBLIC OF)",
    "IRAQ", "JORDAN", "KUWAIT", "LEBANON", "LIBYA", "MOROCCO",
    "OCCUPIED PALESTINIAN TERRITORY, INCLUDING EAST JERUSALEM", "OMAN", "PAKISTAN",
    "QATAR", "SAUDI ARABIA", "SOMALIA", "SUDAN", "SYRIAN ARAB REPUBLIC", "TUNISIA",
    "UNITED ARAB EMIRATES", "YEMEN"
  )

  dplyr::if_else(country_name %in% emro_ctry, "EMRO", "AFRO")
}

#' Normalize regional country names/codes to match sirfunctions behavior
#' @param country_name `chr`
#' @returns `chr`
#' @keywords internal
normalize_regional_country <- function(country_name) {
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

#' Add rolling years for output
#' @param df `tibble`
#' @param start_date `str`
#' @param end_date `str`
#' @param date_col `str`
#' @returns `tibble`
#' @keywords internal
add_rolling_years <- function(df, start_date, end_date, date_col,
                              period = lubridate::years(1)) {
  start_date <- lubridate::as_date(start_date)

  # Normalize input date column defensively to Date
  df <- df |>
    dplyr::mutate(
      !!rlang::sym(date_col) := as.Date(as.character(.data[[date_col]]))
    )

  n_rows <- nrow(df)
  n_row_col <- sum(is.na(df[[date_col]]))

  if (n_rows == n_row_col) {
    cli::cli_abort("The date_col selected is an NA vector. Please check your data.")
  }

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

  df <- adjust_rolling_years(df, end_date, date_col)

  return(df)
}

#' Impute missing country/province/district using AFP line list
#' @param lab_data `tibble`
#' @param afp_data `tibble`
#' @returns `tibble`
#' @keywords internal
impute_missing_lab_geo <- function(lab_data, afp_data = NULL) {
  if (!"EPID" %in% names(lab_data) && "EpidNumber" %in% names(lab_data)) {
    lab_data <- dplyr::rename_with(lab_data, dply::recode, EpidNumber = "EPID")
  }

  if (is.null(afp_data)) {
    lab_data$ctry <- NA
    lab_data$prov <- NA
    lab_data$dist <- NA
    lab_data$adm0guid <- NA
    lab_data$adm1guid <- NA
    lab_data$adm2guid <- NA
    return(lab_data)
  }

  afp_data <- dplyr::rename_with(afp_data, dplyr::recode,
                                 place.admin.0 = "ctry",
                                 place.admin.1 = "prov",
                                 place.admin.2 = "dist"
  )

  if (!"EPID" %in% names(lab_data)) {
    lab_data$ctry <- NA
    lab_data$prov <- NA
    lab_data$dist <- NA
    lab_data$adm0guid <- NA
    lab_data$adm1guid <- NA
    lab_data$adm2guid <- NA
    return(lab_data)
  }

  match_idx <- match(as.character(lab_data$EPID), as.character(afp_data$epid))
  lab_data$ctry <- afp_data$ctry[match_idx]
  lab_data$prov <- afp_data$prov[match_idx]
  lab_data$dist <- afp_data$dist[match_idx]

  lab_data$adm0guid <- afp_data$adm0guid[match_idx]
  lab_data$adm1guid <- afp_data$adm1guid[match_idx]
  lab_data$adm2guid <- afp_data$adm2guid[match_idx]

  lab_data
}

#' Load and normalize lab locations for regional cleaner
#' @param lab_locs_path `str` Path to lab locations CSV.
#' @returns `tibble`
#' @keywords internal
get_lab_locs <- function(lab_locs_path = NULL, use_edav = TRUE) {
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
  if (!"seq.cat" %in% names(lab_data)) {
    lab_data$seq.cat <- NA_character_
  }
  if (!"seq.capacity" %in% names(lab_data)) {
    lab_data$seq.capacity <- NA_character_
  }

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
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)

  lab_data <- lab_data |>
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("Date"),
      \(x) as.Date.character(x, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y"), optional = TRUE)
    ))

  if (!"year" %in% names(lab_data)) {
    lab_data <- lab_data |>
      dplyr::mutate(year = lubridate::year(DateOfOnset))
  }

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
      dplyr::between(year, lubridate::year(start_date), lubridate::year(end_date)),
      CaseOrContact == "1-Case"
    )

  out <- impute_missing_lab_geo(out, afp_data)
  out <- out |>
    dplyr::mutate(whoregion = get_region(ctry))

  if (!is.null(ctry_name)) {
    ctry_name <- normalize_lab_ctry_filter(ctry_name)
    out <- out |>
      dplyr::filter(ctry %in% ctry_name | is.na(ctry))
  }

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
  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date)
  lab_locs <- get_lab_locs(lab_locs_path, use_edav)

  out <- dplyr::rename_with(lab_data, dplyr::recode, Name = "country") |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c(
        "CaseDate", "ParalysisOnsetDate", "DateStoolCollected", "StoolDateSentToLab",
        "DateStoolReceivedinLab", "DateFinalCellCultureResult", "DateFinalrRTPCRResults",
        "ReportDateSequenceResultSent", "DateIsolateRcvdForSeq", "DateLArmIsolate",
        "DateRArmIsolate", "DateofSequencing", "DateNotificationtoHQ"
      )), \(x) as.Date.character(
        x,
        tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y"),
        optional = TRUE
      )),
      country = normalize_regional_country(country),
      country = ifelse(stringr::str_detect(country, "IVOIRE"), "COTE D IVOIRE", country),
      year = lubridate::year(ParalysisOnsetDate),
      whoregion = get_region(country)
    ) |>
    dplyr::left_join(
      lab_locs |> dplyr::select(country, seq.capacity, seq.lab, culture.itd.lab),
      by = "country"
    ) |>
    dplyr::filter(dplyr::between(ParalysisOnsetDate, start_date, end_date)) |>
    dplyr::distinct() |>
    dplyr::filter(SpecimenNumber %in% c(1, 2)) |>
    dplyr::filter(!is.na(EPID)) |>
    dplyr::distinct(EPID, SpecimenNumber, .keep_all = TRUE) |>
    dplyr::mutate(
      days.collect.lab = DateStoolReceivedinLab - DateStoolCollected,
      days.lab.culture = DateFinalCellCultureResult - DateStoolReceivedinLab,
      days.seq.ship = DateIsolateRcvdForSeq - DateFinalCellCultureResult,
      days.seq.rec.res = DateofSequencing - DateIsolateRcvdForSeq,
      days.itd.arriveseq = DateIsolateRcvdForSeq - DateFinalrRTPCRResults,
      days.itd.seqres = DateofSequencing - DateFinalrRTPCRResults,
      days.lab.seq = DateofSequencing - DateStoolReceivedinLab
    ) |>
    dplyr::filter(
      (days.collect.lab >= 0 | is.na(days.collect.lab)) &
        (days.lab.culture >= 0 | is.na(days.lab.culture)) &
        (days.seq.ship >= 0 | is.na(days.seq.ship)) &
        (days.lab.seq >= 0 | is.na(days.lab.seq)) &
        (days.itd.seqres >= 0 | is.na(days.itd.seqres)) &
        (days.itd.arriveseq >= 0 | is.na(days.itd.arriveseq)) &
        (days.seq.rec.res >= 0 | is.na(days.seq.rec.res))
    )

  # Option B: apply stool/onset filter only if it keeps rows
  out_candidate <- out |>
    dplyr::filter(DateStoolCollected >= ParalysisOnsetDate | is.na(ParalysisOnsetDate))

  if (nrow(out_candidate) > 0) {
    out <- out_candidate
  } else {
    cli::cli_alert_warning(
      "DateStoolCollected >= ParalysisOnsetDate filter removed all rows; skipping this filter for current dataset."
    )
  }

  out <- out |>
    dplyr::select(-dplyr::contains("cIntratypeIs"))

  out <- impute_missing_lab_geo(out, afp_data)

  if (!is.null(ctry_name)) {
    ctry_name <- normalize_lab_ctry_filter(ctry_name)
    out <- out |>
      dplyr::filter(ctry %in% ctry_name | is.na(ctry))
  }

  out |>
    dplyr::mutate(
      days.coll.sent.field = NA,
      days.sent.field.rec.nat = NA,
      days.rec.nat.sent.lab = NA,
      days.sent.lab.rec.lab = NA,
      days.rec.lab.culture = NA
    )
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
  out <- if ("MasterKey" %in% names(lab_data)) {
    clean_lab_data_who(lab_data, start_date, end_date, afp_data, ctry_name) |>
      add_rolling_years(start_date, end_date, "DateOfOnset")
  } else {
    clean_lab_data_regional(
      lab_data, start_date, end_date,
      afp_data, ctry_name, lab_locs_path, use_edav
    ) |>
      add_rolling_years(start_date, end_date, "CaseDate")
  }

  out <- apply_lab_cleaning_updates(out)

  if (!"MasterKey" %in% names(lab_data)) {
    if (!"EPID" %in% names(out) && "EpidNumber" %in% names(out)) {
      out$EPID <- as.character(out$EpidNumber)
    }
    if ("EPID" %in% names(out) && "EpidNumber" %in% names(out)) {
      out$EPID <- dplyr::coalesce(as.character(out$EPID), as.character(out$EpidNumber))
    }
    if (!"country" %in% names(out)) {
      out$country <- dplyr::coalesce(
        if ("ctry" %in% names(out)) out$ctry else NA_character_,
        if ("Name" %in% names(out)) out$Name else NA_character_
      )
    } else {
      out$country <- dplyr::coalesce(
        out$country,
        if ("ctry" %in% names(out)) out$ctry else NA_character_,
        if ("Name" %in% names(out)) out$Name else NA_character_
      )
    }
    out$country <- stringr::str_to_upper(out$country)
    if (!"Name" %in% names(out) && "country" %in% names(out)) {
      out$Name <- out$country
    }
    if ("region" %in% names(out)) {
      out$region <- dplyr::coalesce(
        out$region,
        if ("whoregion" %in% names(out)) out$whoregion else NA_character_,
        if ("country" %in% names(out)) get_region(out$country) else NA_character_,
        if ("ctry" %in% names(out)) get_region(out$ctry) else NA_character_,
        if ("Name" %in% names(out)) get_region(out$Name) else NA_character_
      )
    } else {
      out$region <- dplyr::coalesce(
        if ("whoregion" %in% names(out)) out$whoregion else NA_character_,
        if ("country" %in% names(out)) get_region(out$country) else NA_character_,
        if ("ctry" %in% names(out)) get_region(out$ctry) else NA_character_,
        if ("Name" %in% names(out)) get_region(out$Name) else NA_character_
      )
    }
    missing_cols <- setdiff(
      c(
        "EPID", "SpecimenNumber", "CaseContactCode", "Name", "CaseDate",
        "ParalysisOnsetDate", "DateStoolCollected", "StoolDateSentToLab",
        "DateStoolReceivedinLab", "FinalCellCultureResult", "DateFinalCellCultureResult",
        "FinalITDResult", "DateFinalrRTPCRResults", "DateIsolateSentforSequencing",
        "ReportDateSequenceResultSent", "DateIsolateRcvdForSeq", "DateLArmIsolate",
        "DateRArmIsolate", "ResultNPENT", "cIntratypeIsV1", "cIntratypeIsV2",
        "cIntratypeIsV3", "cIntratypeIsVDPV1", "cIntratypeIsVDPV2", "cIntratypeIsVDPV3",
        "cIntratypeIsW1", "cIntratypeIsW2", "cIntratypeIsW3", "DateofSequencing",
        "DateNotificationtoHQ", "WILD1", "VDPV1", "VDPV2", "VDPV3", "region"
      ),
      names(out)
    )
    for (col in missing_cols) {
      out[[col]] <- NA
    }

    out <- out |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c(
            "CaseDate", "ParalysisOnsetDate", "DateStoolCollected", "StoolDateSentToLab",
            "DateStoolReceivedinLab", "DateFinalCellCultureResult", "DateFinalrRTPCRResults",
            "ReportDateSequenceResultSent", "DateIsolateRcvdForSeq", "DateLArmIsolate",
            "DateRArmIsolate", "DateofSequencing"
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
      )

    out <- out |>
      dplyr::select(dplyr::any_of(c(
        "EPID", "SpecimenNumber", "CaseContactCode", "Name", "CaseDate",
        "ParalysisOnsetDate", "DateStoolCollected", "StoolDateSentToLab",
        "DateStoolReceivedinLab", "FinalCellCultureResult", "DateFinalCellCultureResult",
        "FinalITDResult", "DateFinalrRTPCRResults", "DateIsolateSentforSequencing",
        "ReportDateSequenceResultSent", "DateIsolateRcvdForSeq", "DateLArmIsolate",
        "DateRArmIsolate", "ResultNPENT", "cIntratypeIsV1", "cIntratypeIsV2",
        "cIntratypeIsV3", "cIntratypeIsVDPV1", "cIntratypeIsVDPV2", "cIntratypeIsVDPV3",
        "cIntratypeIsW1", "cIntratypeIsW2", "cIntratypeIsW3", "DateofSequencing",
        "DateNotificationtoHQ", "WILD1", "VDPV1", "VDPV2", "VDPV3", "region"
      )))
  }

  # .rda output
  if (!is.null(save_rda_path)) {
    lab_clean_all <- out
    save(lab_clean_all, file = save_rda_path)
  }

  out
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
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop('Package "readxl" must be installed to use this function.',
         .call = FALSE
    )
  }

  if (stringr::str_ends(lab_data_path, ".csv")) {
    return(readr::read_csv(lab_data_path))
  } else if (stringr::str_ends(lab_data_path, ".xlsx")) {
    return(readxl::read_excel(lab_data_path, sheet = sheet_name))
  } else {
    stop("Not a csv or .xlsx file. Please try again.")
  }
}
