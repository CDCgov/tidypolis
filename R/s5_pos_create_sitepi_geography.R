#' Assign situation epidemiology geography for the 2026 boundary update
#'
#' @param pos Combined human and environmental positives, with `dateonset`,
#'   `measurement`, historical geography, and `latitude`/`longitude`. An optional
#'   `collection.date` is also considered when checking detection eligibility.
#' @param province_shapes Province polygons containing `GUID`, `ADM0_GUID`,
#'   `ADM0_NAME`, `ADM1_NAME`, `yr.st`, and `yr.end`.
#' @param district_shapes District polygons containing the same fields plus
#'   `ADM2_NAME`.
#' @param shape_year Replacement boundary start year. Defaults to 2026.
#' @param detection_start First eligible onset or collection date. Defaults to
#'   January 1, 2025.
#' @details All rows initially receive their existing geography in `sitepi_*`.
#' Eligible measurements are cVDPV 1/2/3, VDPV 1/2/3, and WILD 1, across all
#' surveillance sources and countries. Eligible detections in an admin area
#' retired in the preceding year are spatially matched to replacement polygons
#' starting in `shape_year`. Province and district updates use their respective
#' shapefiles independently. Processed POS coordinates, including randomly imputed
#' coordinates, are used. Unresolved district names, including empty or whitespace
#' values, fall back to `place.admin.2`. Unresolved GUIDs and province fields remain
#' `NA`; unaffected levels retain their existing geography. Row order,
#' row count, and historical geography are preserved. Polygons that fail spherical
#' validation are repaired and matched with GEOS; the caller's S2 setting is restored.
#' @returns The positives dataset with four `sitepi_*` geography columns.
#' @keywords internal
s5_pos_create_sitepi_geography <- function(pos, province_shapes, district_shapes,
                                           shape_year = 2026L,
                                           detection_start = as.Date("2025-01-01")) {
  required <- c("dateonset", "measurement", "place.admin.0", "place.admin.1",
                "place.admin.2", "adm0guid", "adm1guid", "admin2guid",
                "latitude", "longitude")
  missing_columns <- setdiff(required, names(pos))
  if (length(missing_columns) > 0) {
    cli::cli_abort("Positives are missing required sitepi fields: {missing_columns}.")
  }

  measurements <- c("cVDPV 1", "cVDPV 2", "cVDPV 3", "VDPV 1", "VDPV 2",
                    "VDPV 3", "WILD 1")
  eligible_date <- !is.na(pos$dateonset) & as.Date(pos$dateonset) >= detection_start
  for (column in intersect(c("collection.date", ".sitepi_collection_date"), names(pos))) {
    collection_date <- as.Date(pos[[column]])
    eligible_date <- eligible_date | (!is.na(collection_date) & collection_date >= detection_start)
  }
  eligible <- eligible_date & pos$measurement %in% measurements

  # Fill unchanged and out-of-scope detections, replacing any stale sitepi fields.
  pos$sitepi_admin1_name <- as.character(pos$place.admin.1)
  pos$sitepi_admin1_guid <- as.character(pos$adm1guid)
  pos$sitepi_admin2_name <- as.character(pos$place.admin.2)
  pos$sitepi_admin2_guid <- as.character(pos$admin2guid)

  pos <- s5_sitepi_update_level(pos, province_shapes, eligible, 1L, shape_year)
  pos <- s5_sitepi_update_level(pos, district_shapes, eligible, 2L, shape_year)
  # Preserve the original district name when the replacement name is unavailable.
  missing_district_name <- is.na(pos$sitepi_admin2_name) |
    trimws(pos$sitepi_admin2_name) == ""
  pos$sitepi_admin2_name[missing_district_name] <-
    as.character(pos$place.admin.2[missing_district_name])
  pos$.sitepi_collection_date <- NULL
  pos
}

s5_sitepi_normalize_guid <- function(value) {
  value <- gsub("[{}]", "", toupper(trimws(as.character(value))))
  value[value %in% c("", "NA", "NULL")] <- NA_character_
  value
}

s5_sitepi_normalize_name <- function(value) {
  value <- toupper(trimws(as.character(value)))
  value[value == ""] <- NA_character_
  value
}

s5_sitepi_update_level <- function(pos, shapes, eligible, level, shape_year) {
  shape_names <- paste0("ADM", 0:level, "_NAME")
  required <- c("GUID", "ADM0_GUID", "yr.st", "yr.end", shape_names)
  if (!inherits(shapes, "sf") || !all(required %in% names(shapes))) {
    cli::cli_abort("Admin {level} boundaries must be sf polygons with fields: {required}.")
  }

  retired <- sf::st_drop_geometry(shapes[which(shapes$yr.end == shape_year - 1L), ])
  if (nrow(retired) == 0 || !any(eligible)) {
    return(pos)
  }

  guid_column <- if (level == 1L) "adm1guid" else "admin2guid"
  # GUIDs identify areas globally. Complete name hierarchies also support records
  # whose GUID is missing or differs from the historical shapefile.
  by_guid <- which(s5_sitepi_normalize_guid(pos[[guid_column]]) %in%
                     stats::na.omit(s5_sitepi_normalize_guid(retired$GUID)))
  name_columns <- paste0("place.admin.", 0:level)
  case_names <- as.data.frame(lapply(pos[name_columns], s5_sitepi_normalize_name))
  retired_names <- as.data.frame(lapply(retired[shape_names], s5_sitepi_normalize_name))
  names(retired_names) <- name_columns
  case_names$.sitepi_row <- seq_len(nrow(pos))
  by_name <- dplyr::semi_join(case_names, retired_names,
                             by = name_columns, na_matches = "never")$.sitepi_row
  affected <- intersect(which(eligible), union(by_guid, by_name))
  if (length(affected) == 0) {
    return(pos)
  }

  name_output <- paste0("sitepi_admin", level, "_name")
  guid_output <- paste0("sitepi_admin", level, "_guid")
  pos[[name_output]][affected] <- NA_character_
  pos[[guid_output]][affected] <- NA_character_

  replacements <- shapes[which(shapes$yr.st == shape_year), ]
  latitude <- suppressWarnings(as.numeric(pos$latitude))
  longitude <- suppressWarnings(as.numeric(pos$longitude))
  usable <- is.finite(latitude) & is.finite(longitude) &
    latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180 &
    !(latitude == 0 & longitude == 0)
  rows <- intersect(affected, which(usable))
  if (length(rows) == 0 || nrow(replacements) == 0) {
    return(pos)
  }

  boundaries <- sf::st_sf(
    .new_name = as.character(replacements[[paste0("ADM", level, "_NAME")]]),
    .new_guid = as.character(replacements$GUID),
    .country_guid = s5_sitepi_normalize_guid(replacements$ADM0_GUID),
    .country_name = s5_sitepi_normalize_name(replacements$ADM0_NAME),
    geometry = sf::st_geometry(replacements)
  )
  points <- sf::st_as_sf(
    data.frame(.sitepi_row = rows, longitude = longitude[rows], latitude = latitude[rows]),
    coords = c("longitude", "latitude"), crs = 4326
  ) |>
    sf::st_transform(sf::st_crs(boundaries))
  matches <- s5_sitepi_join_boundaries(points, boundaries, level)

  # A coordinate must resolve within the detection's country as well as its polygon.
  country_guid <- s5_sitepi_normalize_guid(pos$adm0guid)[matches$.sitepi_row]
  country_name <- s5_sitepi_normalize_name(pos$place.admin.0)[matches$.sitepi_row]
  same_country <- ifelse(
    !is.na(country_guid) & !is.na(matches$.country_guid),
    country_guid == matches$.country_guid,
    country_name == matches$.country_name
  )
  matches <- matches[which(same_country), c(".sitepi_row", ".new_name", ".new_guid")] |>
    dplyr::distinct()
  counts <- table(matches$.sitepi_row)
  ambiguous <- as.integer(names(counts[counts > 1L]))
  if (length(ambiguous) > 0) {
    cli::cli_warn(paste0(
      "Admin ", level, " replacement geography is ambiguous for ", length(ambiguous),
      " detection(s); no replacement has been assigned for this level."
    ))
  }
  matches <- matches[!matches$.sitepi_row %in% ambiguous, ]
  pos[[name_output]][matches$.sitepi_row] <- matches$.new_name
  pos[[guid_output]][matches$.sitepi_row] <- matches$.new_guid
  pos
}

# Keep spherical matching for valid polygons. A spherical rebuild can leave
# self-crossings behind, so use GEOS for those features and for failed S2 joins.
s5_sitepi_join_boundaries <- function(points, boundaries, level) {
  initial_s2 <- sf::sf_use_s2()
  on.exit({
    if (!identical(sf::sf_use_s2(), initial_s2)) {
      suppressMessages(sf::sf_use_s2(initial_s2))
    }
  }, add = TRUE)

  if (!initial_s2 || !isTRUE(sf::st_is_longlat(boundaries))) {
    return(sf::st_drop_geometry(suppressMessages(sf::st_join(
      points, sf::st_make_valid(boundaries), left = FALSE
    ))))
  }

  spherical <- tryCatch(sf::st_make_valid(boundaries), error = function(e) boundaries)
  valid <- tryCatch(sf::st_is_valid(spherical),
                     error = function(e) rep(FALSE, nrow(boundaries)))
  spherical_rows <- which(valid %in% TRUE)
  geos_rows <- setdiff(seq_len(nrow(boundaries)), spherical_rows)
  matches <- NULL
  if (length(spherical_rows) > 0) {
    matches <- tryCatch(
      sf::st_drop_geometry(sf::st_join(
        points, spherical[spherical_rows, ], left = FALSE, model = "closed"
      )),
      error = function(e) e
    )
    if (inherits(matches, "error")) {
      # Conversion during the join may reject a geometry that passed validation.
      geos_rows <- seq_len(nrow(boundaries))
      matches <- NULL
    }
  }

  if (length(geos_rows) > 0) {
    cli::cli_alert_info(paste0(
      "Admin ", level, ": using GEOS repair and matching for ", length(geos_rows),
      " replacement polygon(s) that failed spherical validation."
    ))
    suppressMessages(sf::sf_use_s2(FALSE))
    # Repair the original features with GEOS, rather than reusing an invalid
    # spherical rebuild. Never change the stored shapefile or its attributes.
    repaired <- sf::st_make_valid(boundaries[geos_rows, ])
    geos_matches <- sf::st_drop_geometry(suppressMessages(sf::st_join(
      points, repaired, left = FALSE
    )))
    matches <- dplyr::bind_rows(matches, geos_matches)
  }

  matches
}
