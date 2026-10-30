sitepi_box <- function(xmin, xmax, ymin = 10, ymax = 12) {
  sf::st_polygon(list(matrix(c(
    xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin
  ), ncol = 2, byrow = TRUE)))
}

sitepi_crossing_polygon <- function() {
  sf::st_polygon(list(matrix(c(
    10, 10, 11, 12, 10, 12, 11, 10, 10, 10
  ), ncol = 2, byrow = TRUE)))
}

sitepi_shapes <- function() {
  province <- sf::st_sf(
    ADM0_NAME = "GUINEA BISSAU", ADM0_GUID = "{COUNTRY}",
    ADM1_NAME = c("Gabu", "Gabu 2026", "Unchanged province", "Future province"),
    GUID = c("{OLD-PROV}", "{NEW-PROV}", "{UNCHANGED-PROV}", "{FUTURE-PROV}"),
    yr.st = c(2000L, 2026L, 1980L, 2027L),
    yr.end = c(2025L, 9999L, 9999L, 9999L),
    Shape = sf::st_sfc(sitepi_box(10, 12), sitepi_box(10, 12),
                       sitepi_box(20, 22), sitepi_box(10, 12), crs = 4326)
  )
  district <- sf::st_sf(
    ADM0_NAME = "GUINEA BISSAU", ADM0_GUID = "{COUNTRY}",
    ADM1_NAME = c("Gabu", "Stale province", "Stale province", "Unchanged province", "Future"),
    ADM1_GUID = c("{OLD-PROV}", "{STALE}", "{STALE}", "{UNCHANGED-PROV}", "{FUTURE-PROV}"),
    ADM2_NAME = c("Gabu", "Gabu West", "Gabu East", "Unchanged district", "Future district"),
    GUID = c("{OLD-DIST}", "{WEST}", "{EAST}", "{UNCHANGED-DIST}", "{FUTURE-DIST}"),
    yr.st = c(2000L, 2026L, 2026L, 1980L, 2027L),
    yr.end = c(2025L, 9999L, 9999L, 9999L, 9999L),
    Shape = sf::st_sfc(sitepi_box(10, 12), sitepi_box(10, 11), sitepi_box(11, 12),
                       sitepi_box(20, 22), sitepi_box(10, 12), crs = 4326)
  )
  list(province = province, district = district)
}

sitepi_pos <- function() {
  dplyr::tibble(
    epid = c("afp", "contact", "es", "unchanged"),
    virus.id = seq_len(4), source = c("AFP", "Contact", "ENV", "Community"),
    dateonset = as.Date("2025-01-01"), yronset = 2025L, measurement = "cVDPV 2",
    place.admin.0 = "GUINEA BISSAU",
    place.admin.1 = c(rep("Gabu", 3), "Unchanged province"),
    place.admin.2 = c(rep("Gabu", 3), "Unchanged district"),
    adm0guid = "{COUNTRY}",
    adm1guid = c(rep("{OLD-PROV}", 3), "{UNCHANGED-PROV}"),
    admin2guid = c(rep("{OLD-DIST}", 3), "{UNCHANGED-DIST}"),
    latitude = "10.5", longitude = c("10.5", "11.5", "10.5", "20.5")
  )
}

sitepi_apply <- function(pos = sitepi_pos(), shapes = sitepi_shapes()) {
  s5_pos_create_sitepi_geography(pos, shapes$province, shapes$district)
}

test_that("all sources use independent 2026 province and district boundaries", {
  pos <- sitepi_pos()
  result <- sitepi_apply(pos)
  expect_equal(result$sitepi_admin1_name, c(rep("Gabu 2026", 3), "Unchanged province"))
  expect_equal(result$sitepi_admin1_guid, c(rep("{NEW-PROV}", 3), "{UNCHANGED-PROV}"))
  expect_equal(result$sitepi_admin2_name, c("Gabu West", "Gabu East", "Gabu West", "Unchanged district"))
  expect_equal(result$sitepi_admin2_guid, c("{WEST}", "{EAST}", "{WEST}", "{UNCHANGED-DIST}"))
  expect_equal(result[names(pos)], pos)
})

test_that("January 1 2025 is inclusive and earlier or undated detections pass through", {
  pos <- sitepi_pos()[rep(1, 4), ]
  pos$dateonset <- as.Date(c("2024-12-31", "2025-01-01", "2026-01-01", NA))
  # Eligibility follows the detection date, rather than a potentially stale year field.
  pos$yronset <- 2024L
  result <- sitepi_apply(pos)
  expect_equal(result$sitepi_admin1_guid, c("{OLD-PROV}", "{NEW-PROV}", "{NEW-PROV}", "{OLD-PROV}"))
  expect_equal(result$sitepi_admin2_guid, c("{OLD-DIST}", "{WEST}", "{WEST}", "{OLD-DIST}"))
  expect_equal(nrow(result), 4L)
})

test_that("only the seven specified measurements are reassigned", {
  types <- c("cVDPV 1", "cVDPV 2", "cVDPV 3", "VDPV 1", "VDPV 2", "VDPV 3", "WILD 1",
             "aVDPV 2", "iVDPV 2", "VACCINE 2", "WILD 3", NA)
  pos <- sitepi_pos()[rep(1, length(types)), ]
  pos$measurement <- types
  result <- sitepi_apply(pos)
  expect_equal(result$sitepi_admin1_guid[1:7], rep("{NEW-PROV}", 7))
  expect_equal(result$sitepi_admin2_guid[1:7], rep("{WEST}", 7))
  expect_equal(result$sitepi_admin1_guid[8:12], rep("{OLD-PROV}", 5))
  expect_equal(result$sitepi_admin2_guid[8:12], rep("{OLD-DIST}", 5))
})

test_that("collection dates also qualify environmental detections", {
  pos <- sitepi_pos()[3, ]
  pos$dateonset <- as.Date(NA)
  pos$collection.date <- as.Date("2025-01-01")
  result <- sitepi_apply(pos)
  expect_equal(result$sitepi_admin2_guid, "{WEST}")
  expect_true(is.na(result$dateonset))
  expect_equal(result$collection.date, pos$collection.date)

  pos$collection.date <- NULL
  pos$.sitepi_collection_date <- as.POSIXct("2025-01-01", tz = "UTC")
  result <- sitepi_apply(pos)
  expect_equal(result$sitepi_admin2_guid, "{WEST}")
  expect_false(".sitepi_collection_date" %in% names(result))
})

test_that("unchanged areas and excluded detections are filled without coordinates", {
  pos <- sitepi_pos()[c(4, 1, 2), ]
  pos$dateonset[2] <- as.Date("2024-12-31")
  pos$measurement[3] <- "VACCINE 2"
  pos$latitude <- pos$longitude <- NA_character_
  pos$sitepi_admin1_name <- "Stale name"
  pos$sitepi_admin2_guid <- "{STALE}"
  result <- sitepi_apply(pos)
  expect_equal(result$sitepi_admin1_name, pos$place.admin.1)
  expect_equal(result$sitepi_admin1_guid, pos$adm1guid)
  expect_equal(result$sitepi_admin2_name, pos$place.admin.2)
  expect_equal(result$sitepi_admin2_guid, pos$admin2guid)
})

test_that("province-only and district-only changes are handled independently", {
  shapes <- sitepi_shapes()
  shapes$district$yr.end[1] <- 9999L
  result <- sitepi_apply(sitepi_pos()[1, ], shapes)
  expect_equal(result$sitepi_admin1_guid, "{NEW-PROV}")
  expect_equal(result$sitepi_admin2_guid, "{OLD-DIST}")

  shapes <- sitepi_shapes()
  shapes$province$yr.end[1] <- 9999L
  result <- sitepi_apply(sitepi_pos()[1, ], shapes)
  expect_equal(result$sitepi_admin1_guid, "{OLD-PROV}")
  expect_equal(result$sitepi_admin2_guid, "{WEST}")
})

test_that("only areas retired in 2025 and replacements starting in 2026 are used", {
  shapes <- sitepi_shapes()
  shapes$province$yr.end[1] <- 2024L
  shapes$district$yr.end[1] <- 2026L
  result <- sitepi_apply(sitepi_pos()[1, ], shapes)
  expect_equal(result$sitepi_admin1_guid, "{OLD-PROV}")
  expect_equal(result$sitepi_admin2_guid, "{OLD-DIST}")

  shapes <- sitepi_shapes()
  shapes$province$yr.st[2] <- 2027L
  shapes$district$yr.st[2:3] <- 2027L
  result <- sitepi_apply(sitepi_pos()[1, ], shapes)
  expect_true(is.na(result$sitepi_admin1_guid))
  expect_true(is.na(result$sitepi_admin2_guid))
  expect_equal(result$sitepi_admin2_name, "Gabu")
})

test_that("GUID matching tolerates case and braces even when names are missing", {
  pos <- sitepi_pos()[1, ]
  pos$adm0guid <- "country"
  pos$adm1guid <- "old-prov"
  pos$admin2guid <- "old-dist"
  pos$place.admin.1 <- pos$place.admin.2 <- NA_character_
  result <- sitepi_apply(pos)
  expect_equal(result$sitepi_admin1_guid, "{NEW-PROV}")
  expect_equal(result$sitepi_admin2_guid, "{WEST}")
})

test_that("complete name hierarchies support missing GUIDs without cross-country matches", {
  pos <- sitepi_pos()[rep(1, 3), ]
  pos$adm1guid <- pos$admin2guid <- NA_character_
  pos$place.admin.0[2] <- "OTHER COUNTRY"
  pos$place.admin.1[3] <- "Another province"
  result <- sitepi_apply(pos)
  expect_equal(result$sitepi_admin1_guid[1], "{NEW-PROV}")
  expect_equal(result$sitepi_admin2_guid[1], "{WEST}")
  expect_equal(result$sitepi_admin1_name[2:3], c("Gabu", "Another province"))
  expect_equal(result$sitepi_admin2_name[2:3], c("Gabu", "Gabu"))
  expect_true(all(is.na(result$sitepi_admin2_guid[2:3])))
})

test_that("foreign polygons do not create false ambiguity in worldwide files", {
  shapes <- sitepi_shapes()
  foreign <- shapes$district[2, ]
  foreign$ADM0_NAME <- "OTHER COUNTRY"
  foreign$ADM0_GUID <- "{OTHER-COUNTRY}"
  foreign$GUID <- "{FOREIGN}"
  shapes$district <- rbind(shapes$district, foreign)
  result <- sitepi_apply(sitepi_pos()[1, ], shapes)
  expect_equal(result$sitepi_admin2_guid, "{WEST}")
})

test_that("retired areas in additional countries receive their own replacements", {
  shapes <- sitepi_shapes()
  province <- shapes$province[1:2, ]
  province$ADM0_NAME <- "SECOND COUNTRY"
  province$ADM0_GUID <- "{SECOND-COUNTRY}"
  province$GUID <- c("{SECOND-OLD-PROV}", "{SECOND-NEW-PROV}")
  sf::st_geometry(province) <- sf::st_sfc(sitepi_box(30, 32), sitepi_box(30, 32), crs = 4326)
  district <- shapes$district[1:2, ]
  district$ADM0_NAME <- "SECOND COUNTRY"
  district$ADM0_GUID <- "{SECOND-COUNTRY}"
  district$GUID <- c("{SECOND-OLD-DIST}", "{SECOND-NEW-DIST}")
  sf::st_geometry(district) <- sf::st_sfc(sitepi_box(30, 32), sitepi_box(30, 32), crs = 4326)
  shapes$province <- rbind(shapes$province, province)
  shapes$district <- rbind(shapes$district, district)
  pos <- sitepi_pos()[1:2, ]
  pos$place.admin.0[2] <- "SECOND COUNTRY"
  pos$adm0guid[2] <- "{SECOND-COUNTRY}"
  pos$adm1guid[2] <- "{SECOND-OLD-PROV}"
  pos$admin2guid[2] <- "{SECOND-OLD-DIST}"
  pos$longitude[2] <- "30.5"
  result <- sitepi_apply(pos, shapes)
  expect_equal(result$sitepi_admin1_guid, c("{NEW-PROV}", "{SECOND-NEW-PROV}"))
  expect_equal(result$sitepi_admin2_guid, c("{WEST}", "{SECOND-NEW-DIST}"))
})

test_that("multiple detections sharing an EPID retain separate eligibility and locations", {
  pos <- sitepi_pos()[c(1, 2, 1), ]
  pos$epid <- "same-epid"
  pos$virus.id <- c(91, 92, 93)
  pos$dateonset[3] <- as.Date("2024-01-01")
  result <- sitepi_apply(pos)
  expect_equal(result$sitepi_admin2_guid, c("{WEST}", "{EAST}", "{OLD-DIST}"))
  expect_equal(result$virus.id, pos$virus.id)
  expect_equal(result[names(pos)], pos)
})

test_that("duplicate shape records do not multiply detections", {
  shapes <- sitepi_shapes()
  shapes$province <- rbind(shapes$province, shapes$province[2, ])
  shapes$district <- rbind(shapes$district, shapes$district[2, ])
  result <- sitepi_apply(shapes = shapes)
  expect_equal(nrow(result), 4L)
  expect_equal(result$sitepi_admin2_guid[1], "{WEST}")
})

test_that("ambiguous district matches retain the original district name", {
  shapes <- sitepi_shapes()
  overlap <- shapes$district[2, ]
  overlap$GUID <- "{OVERLAP}"
  shapes$district <- rbind(shapes$district, overlap)
  expect_warning(result <- sitepi_apply(sitepi_pos()[1, ], shapes), "ambiguous")
  expect_true(is.na(result$sitepi_admin2_guid))
  expect_equal(result$sitepi_admin2_name, "Gabu")
  expect_equal(result$sitepi_admin1_guid, "{NEW-PROV}")
  expect_equal(nrow(result), 1L)
})

test_that("unusable or unmatched points retain the original district name", {
  pos <- sitepi_pos()[rep(1, 5), ]
  pos$latitude <- c(NA, "0", "91", "Inf", "30")
  pos$longitude <- c("10.5", "0", "10.5", "10.5", "30")
  result <- sitepi_apply(pos)
  expect_true(all(is.na(result$sitepi_admin1_guid)))
  expect_true(all(is.na(result$sitepi_admin2_guid)))
  expect_equal(result$sitepi_admin2_name, pos$place.admin.2)
  expect_equal(nrow(result), 5L)
})

test_that("missing or blank replacement district names fall back without changing valid matches", {
  for (missing_name in c(NA_character_, "", " \t ")) {
    shapes <- sitepi_shapes()
    shapes$district$ADM2_NAME[2] <- missing_name
    pos <- sitepi_pos()[1:3, ]
    result <- sitepi_apply(pos, shapes)
    expect_equal(result$sitepi_admin2_name, c("Gabu", "Gabu East", "Gabu"))
    expect_equal(result$sitepi_admin2_guid, c("{WEST}", "{EAST}", "{WEST}"))
    expect_equal(result[names(pos)], pos)
  }
  pos <- sitepi_pos()[1, ]
  pos$latitude <- NA_character_
  pos$place.admin.2 <- NA_character_
  result <- sitepi_apply(pos)
  expect_true(is.na(result$sitepi_admin2_name))
})

test_that("projected shapefiles and empty positives are supported", {
  shapes <- lapply(sitepi_shapes(), sf::st_transform, crs = 3857)
  result <- sitepi_apply(sitepi_pos()[1, ], shapes)
  expect_equal(result$sitepi_admin1_guid, "{NEW-PROV}")
  expect_equal(result$sitepi_admin2_guid, "{WEST}")
  empty <- sitepi_apply(sitepi_pos()[0, ])
  expect_equal(nrow(empty), 0L)
  expect_true(all(vapply(empty[grep("^sitepi_", names(empty))], is.character, logical(1))))
})

test_that("random points created by Step 2 can receive the 2026 geography in Step 5", {
  withr::local_seed(123)
  cases <- dplyr::tibble(
    epid = c("imputed-1", "imputed-2"), yronset = 2025L,
    place.admin.0 = "GUINEA BISSAU", place.admin.1 = "Gabu", place.admin.2 = "Gabu",
    admin0guid = "country", Admin0GUID = "{COUNTRY}",
    admin1guid = "old-prov", Admin1GUID = "{OLD-PROV}",
    admin2guid = "old-dist", Admin2GUID = "{OLD-DIST}",
    polis.latitude = NA_real_, polis.longitude = NA_real_
  )
  sampled <- suppressMessages(f.pre.stsample.01(cases, sitepi_shapes()$district))
  expect_equal(nrow(sampled), 2L)
  expect_true(all(is.finite(sampled$lat) & is.finite(sampled$lon)))
  expect_false(any(grepl("^sitepi_", names(sampled))))
  pos <- sitepi_pos()[rep(1, nrow(sampled)), ]
  pos$epid <- sampled$epid
  pos$latitude <- sampled$lat
  pos$longitude <- sampled$lon
  result <- sitepi_apply(pos)
  expect_equal(result$sitepi_admin1_guid, rep("{NEW-PROV}", 2))
  expect_true(all(result$sitepi_admin2_guid %in% c("{WEST}", "{EAST}")))
})

test_that("Step 5 assigns sitepi after combining human and environmental records", {
  pos <- sitepi_pos()
  wire <- pos |>
    dplyr::rename(surveillance.type = source, virustypename = measurement, adm2guid = admin2guid) |>
    dplyr::mutate(
      vdpvclassificationchangedate = as.POSIXct(NA),
      datenotificationtohq = as.POSIXct(NA), env.sample.id = NA_character_,
      polis.case.id = NA_character_
    )
  human <- wire[wire$surveillance.type != "ENV", ]
  env <- wire[wire$surveillance.type == "ENV", ]
  env$.sitepi_collection_date <- as.POSIXct("2025-01-01", tz = "UTC")
  env$dateonset <- as.Date(NA)
  saved <- new.env(parent = emptyenv())
  shapes <- sitepi_shapes()
  local_mocked_bindings(
    s5_pos_load_data = function(...) NULL,
    s5_pos_create_cdc_vars = function(...) NULL,
    s5_pos_check_duplicates = function(...) NULL,
    s5_pos_write_missing_onsets = function(...) NULL,
    s5_pos_process_human_virus = function(...) human,
    s5_pos_process_es_virus = function(...) env,
    remove_character_dates = function(type, df, ...) df,
    create_response_vars = function(pos, ...) pos,
    s5_pos_compare_with_archive = function(afp.es.virus.01, afp.es.virus.03, ...) {
      saved$pos <- afp.es.virus.03
    },
    s5_pos_evaluate_unmatched_guids = function(...) NULL,
    update_polis_log = function(...) NULL,
    tidypolis_io = function(io, file_path, ...) {
      expect_equal(io, "read")
      if (file_path == "POLIS/misc/global.prov.rds") return(shapes$province)
      if (file_path == "POLIS/misc/global.dist.rds") return(shapes$district)
      stop("Unexpected file read")
    },
    .package = "tidypolis"
  )
  suppressMessages(s5_fully_process_pos_data(
    "POLIS", NULL, NULL, output_folder_name = "Core_Ready_Files", output_format = ".rds"
  ))
  result <- saved$pos[match(pos$epid, saved$pos$epid), ]
  expect_equal(nrow(saved$pos), nrow(pos))
  expect_equal(result$sitepi_admin2_guid, c("{WEST}", "{EAST}", "{WEST}", "{UNCHANGED-DIST}"))
  expect_equal(result$sitepi_admin1_guid, c(rep("{NEW-PROV}", 3), "{UNCHANGED-PROV}"))
  expect_false(".sitepi_collection_date" %in% names(saved$pos))
})

test_that("ES processing carries the collection date into eligibility when onset is absent", {
  pos <- sitepi_pos()[3, ]
  virus <- pos |>
    dplyr::rename(surveillance.type = source, virustypename = measurement, adm2guid = admin2guid) |>
    dplyr::mutate(
      virustype = "VDPV 2", dateonset = as.Date(NA), x = 10.5, y = 10.5,
      vdpvclassificationchangedate = as.POSIXct(NA), polis.case.id = NA_character_,
      env.sample.id = "1"
    ) |>
    dplyr::select(-latitude, -longitude)
  es <- dplyr::tibble(
    env.sample.manual.edit.id = "sample-1", env.sample.id = "es", site.id = "site-1",
    ADM0_NAME = "GUINEA BISSAU", ADM1_NAME = "Gabu", ADM2_NAME = "Gabu",
    GUID = "{OLD-DIST}", prov.guid = "{OLD-PROV}", ctry.guid = "{COUNTRY}",
    collection.date = "2025-01-01", collect.yr = 2025L, region.name = "AFRO",
    virus.type = "VDPV 2", date.notification.to.hq = "2025-02-01", lat = "10.5", lng = "10.5"
  )
  local_mocked_bindings(
    tidypolis_io = function(io, ...) {
      if (io == "list") "cache/Core_Ready_Files/es_test.rds" else es
    },
    .package = "tidypolis"
  )
  env <- suppressMessages(s5_pos_process_es_virus(virus, "cache", "Core_Ready_Files", ".rds"))
  final <- suppressMessages(s5_pos_create_final_virus_data(env[0, ], env))
  result <- sitepi_apply(final)
  expect_equal(result$sitepi_admin1_guid, "{NEW-PROV}")
  expect_equal(result$sitepi_admin2_guid, "{WEST}")
  expect_true(is.na(result$dateonset))
  expect_false(".sitepi_collection_date" %in% names(result))
})

test_that("Step 5 accepts human linelists without sitepi columns", {
  cases <- sitepi_pos()[c(1, 2, 1), ] |>
    dplyr::rename(admin0guid = adm0guid, admin1guid = adm1guid) |>
    dplyr::mutate(
      dplyr::across(c(admin0guid, admin1guid, admin2guid),
                    ~ tolower(gsub("[{}]", "", .))),
      epid = c("afp", "contact", "congo"),
      place.admin.0 = c("GUINEA BISSAU", "GUINEA BISSAU", "CONGO"),
      dateonset = as.Date(c("2025-01-01", "2025-01-01", "2010-01-01")),
      yronset = c(2025L, 2025L, 2010L),
      cdc.classification.all = c("VDPV 2", "VDPV 2", "WILD 1"),
      whoregion = "AFRO", nt.changes = 7, emergence.group = "test", virus.cluster = "test",
      surveillancetypename = c("AFP", "Contact", "AFP"),
      lat = as.numeric(latitude), lon = as.numeric(longitude),
      vtype.fixed = c("cVDPV 2", "cVDPV 2", "WILD 1"),
      datenotificationtohq = "2025-02-01"
    )
  files <- c(
    "cache/Core_Ready_Files/afp_linelist_2001-01-01_test.rds",
    "cache/Core_Ready_Files/other_surveillance_type_linelist_2016_test.rds"
  )
  local_mocked_bindings(
    tidypolis_io = function(io, file_path, ...) {
      if (io == "list") return(files)
      if (file_path == files[1]) return(dplyr::filter(cases, surveillancetypename == "AFP"))
      if (file_path == files[2]) return(dplyr::filter(cases, surveillancetypename != "AFP"))
      stop("Unexpected file read")
    },
    .package = "tidypolis"
  )
  virus <- cases |>
    dplyr::transmute(
      epid, surveillance.type = surveillancetypename,
      virustype = cdc.classification.all, virustypename = vtype.fixed,
      dateonset, yronset, place.admin.0, place.admin.1, place.admin.2,
      adm0guid = paste0("{", toupper(admin0guid), "}"),
      adm1guid = paste0("{", toupper(admin1guid), "}"),
      adm2guid = paste0("{", toupper(admin2guid), "}"),
      x = lat, y = lon, env.sample.id = NA_character_, polis.case.id = NA_character_,
      vdpvclassificationchangedate = as.POSIXct(NA)
    )
  human <- suppressMessages(s5_pos_process_human_virus(virus, "cache", "Core_Ready_Files", ".rds"))
  final <- suppressMessages(s5_pos_create_final_virus_data(human, human[0, ]))
  result <- sitepi_apply(final)
  result <- result[match(cases$epid, result$epid), ]
  expect_equal(nrow(result), 3L)
  expect_equal(result$sitepi_admin2_guid, c("{WEST}", "{EAST}", "{OLD-DIST}"))
  expect_equal(result$sitepi_admin1_guid, c("{NEW-PROV}", "{NEW-PROV}", "{OLD-PROV}"))
})

test_that("polygons still invalid after S2 repair use GEOS without affecting valid polygons", {
  initial_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(initial_s2)))
  suppressMessages(sf::sf_use_s2(TRUE))
  shapes <- sitepi_shapes()
  # A self-crossing ring, with one point inside its lower triangle.
  sf::st_geometry(shapes$district)[2] <- sf::st_sfc(sitepi_crossing_polygon(), crs = 4326)
  pos <- sitepi_pos()[1:2, ]
  pos$latitude[1] <- pos$longitude[1] <- "10.25"
  native_make_valid <- sf::st_make_valid
  native_join <- sf::st_join
  geos_guids <- character()
  local_mocked_bindings(
    # Reproduce a spherical repair that leaves the crossing unresolved.
    st_make_valid = function(x, ...) {
      if (sf::sf_use_s2()) x else native_make_valid(x, ...)
    },
    st_join = function(x, y, ...) {
      if (!sf::sf_use_s2()) geos_guids <<- c(geos_guids, y$.new_guid)
      native_join(x, y, ...)
    },
    .package = "sf"
  )
  result <- suppressMessages(sitepi_apply(pos, shapes))
  expect_equal(result$sitepi_admin2_guid, c("{WEST}", "{EAST}"))
  expect_equal(result$sitepi_admin1_guid, rep("{NEW-PROV}", 2))
  expect_equal(unique(geos_guids), "{WEST}")
  expect_equal(result[names(pos)], pos)
  expect_true(sf::sf_use_s2())
})

test_that("errors during spherical repair also fall back to the original polygon", {
  initial_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(initial_s2)))
  suppressMessages(sf::sf_use_s2(TRUE))
  shapes <- sitepi_shapes()
  sf::st_geometry(shapes$district)[2] <- sf::st_sfc(sitepi_crossing_polygon(), crs = 4326)
  pos <- sitepi_pos()[1, ]
  pos$latitude <- pos$longitude <- "10.25"
  native_make_valid <- sf::st_make_valid
  local_mocked_bindings(
    st_make_valid = function(x, ...) {
      if (sf::sf_use_s2()) stop("Loop 0 is not valid: Edge 0 crosses edge 2")
      native_make_valid(x, ...)
    },
    .package = "sf"
  )
  result <- suppressMessages(sitepi_apply(pos, shapes))
  expect_equal(result$sitepi_admin2_guid, "{WEST}")
  expect_equal(result$sitepi_admin1_guid, "{NEW-PROV}")
  expect_true(sf::sf_use_s2())
})

test_that("S2 join validation errors are retried with GEOS", {
  initial_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(initial_s2)))
  suppressMessages(sf::sf_use_s2(TRUE))
  native_join <- sf::st_join
  local_mocked_bindings(
    st_join = function(x, y, ...) {
      if (sf::sf_use_s2()) stop("Loop 0 is not valid: Edge 387 crosses edge 392")
      native_join(x, y, ...)
    },
    .package = "sf"
  )
  result <- suppressMessages(sitepi_apply())
  expect_equal(result$sitepi_admin2_guid, c("{WEST}", "{EAST}", "{WEST}", "{UNCHANGED-DIST}"))
  expect_equal(result$sitepi_admin1_guid, c(rep("{NEW-PROV}", 3), "{UNCHANGED-PROV}"))
  expect_true(sf::sf_use_s2())
})

test_that("S2 is restored even if the GEOS retry fails", {
  initial_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(initial_s2)))
  suppressMessages(sf::sf_use_s2(TRUE))
  local_mocked_bindings(
    st_join = function(...) {
      if (sf::sf_use_s2()) stop("Loop 0 is not valid: Edge 387 crosses edge 392")
      stop("GEOS retry failed")
    },
    .package = "sf"
  )
  expect_error(suppressMessages(sitepi_apply()), "GEOS retry failed")
  expect_true(sf::sf_use_s2())
})

test_that("callers that disable S2 keep their setting after geography assignment", {
  initial_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(initial_s2)))
  suppressMessages(sf::sf_use_s2(FALSE))
  result <- suppressMessages(sitepi_apply())
  expect_equal(result$sitepi_admin2_guid, c("{WEST}", "{EAST}", "{WEST}", "{UNCHANGED-DIST}"))
  expect_false(sf::sf_use_s2())
})
