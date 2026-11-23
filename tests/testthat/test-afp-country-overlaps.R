afp_overlap_box <- function(xmin, xmax, ymin, ymax) {
  sf::st_polygon(list(matrix(c(
    xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin
  ), ncol = 2, byrow = TRUE)))
}

afp_overlap_shapes <- function() {
  sf::st_sf(
    ADM0_NAME = c("INDIA", "CHINA", "INDIA", "PAKISTAN"),
    ADM0_GUID = paste0("{", c(
      "E75886D8-11CE-4563-BD85-86BCB9312CEE", "AF40DA08-EEB3-40E9-BD59-41C06C17191F",
      "E75886D8-11CE-4563-BD85-86BCB9312CEE", "30EE6F01-1785-4ECB-B194-BA3BECF5FD33"
    ), "}"),
    ADM1_NAME = c("ARUNACHAL PR.", "XIZANG", "LADAKH", "GBALTISTAN"),
    ADM1_GUID = c("{IN-AR}", "{CN-XZ}", "{IN-LA}", "{PK-GB}"),
    ADM2_NAME = c("KURUNG KUMEY", "SHANNAN DIQU", "KARGIL", "SKARDU"),
    GUID = paste0("{", c(
      "F3ACAB2A-DB68-4A47-B6CF-5523A43AA9EB", "1780D1AA-DAF6-4957-A353-1ECCA0AE228E",
      "E2AF95AC-5D5C-4B62-8C81-24553F3FD58A", "8DFF32A5-29FF-46F6-B961-6AD5BAA0D227"
    ), "}"),
    yr.st = c(2020L, 2020L, 2020L, 2017L), yr.end = 9999L,
    Shape = sf::st_sfc(
      afp_overlap_box(92, 94, 27, 29), afp_overlap_box(92, 94, 27, 29),
      afp_overlap_box(75, 77, 34, 36), afp_overlap_box(75, 77, 34, 36), crs = 4326
    )
  )
}

afp_overlap_cases <- function() {
  shapes <- sf::st_drop_geometry(afp_overlap_shapes())
  dplyr::tibble(
    epid = c("540629005-2025-00195", "PB/24/23/102"), yronset = c(2025L, 2023L),
    place.admin.0 = c("CHINA", "PAKISTAN"),
    place.admin.1 = c("XIZANG", "GBALTISTAN"), place.admin.2 = NA_character_,
    admin0guid = tolower(gsub("[{}]", "", shapes$ADM0_GUID[c(2, 4)])),
    admin1guid = c("cn-xz", "pk-gb"),
    # Pakistan's existing district points to India: country must take priority.
    admin2guid = c(NA_character_, gsub("[{}]", "", shapes$GUID[3])),
    Admin0GUID = shapes$ADM0_GUID[c(2, 4)], Admin1GUID = shapes$ADM1_GUID[c(2, 4)],
    Admin2GUID = c(NA_character_, shapes$GUID[3]),
    polis.latitude = c("28.109931712", "34.811479252"),
    polis.longitude = c("93.3413299860001", "75.597550132")
  )
}

afp_overlap_apply <- function(cases = afp_overlap_cases(), shapes = afp_overlap_shapes()) {
  suppressMessages(f.pre.stsample.01(cases, shapes))
}

test_that("overlapping India polygons cannot replace China or Pakistan", {
  input <- afp_overlap_cases()
  shapes <- afp_overlap_shapes()
  for (order in list(1:4, 4:1)) {
    result <- afp_overlap_apply(input, shapes[order, ])
    expect_equal(result$epid, input$epid)
    expect_equal(result$place.admin.0, c("CHINA", "PAKISTAN"))
    expect_equal(result$place.admin.1, c("XIZANG", "GBALTISTAN"))
    expect_equal(result$place.admin.2, c("SHANNAN DIQU", "SKARDU"))
    expect_equal(result$Admin0GUID, input$Admin0GUID)
    expect_equal(result$Admin2GUID, shapes$GUID[c(2, 4)])
    expect_equal(result$admin2guid, input$admin2guid)
    expect_equal(result$lat, as.numeric(input$polis.latitude))
    expect_equal(result$lon, as.numeric(input$polis.longitude))
    expect_equal(result$geo.corrected, c(1L, 1L))
    expect_true(all(is.na(result$geo.review)))
    expect_false(any(grepl("^\\.geo_", names(result))))
  }
})

test_that("missing district GUIDs do not prevent coordinate-based matching", {
  cases <- afp_overlap_cases()
  cases$Admin2GUID <- cases$admin2guid <- NA_character_
  result <- afp_overlap_apply(cases)
  expect_equal(result$Admin2GUID, afp_overlap_shapes()$GUID[c(2, 4)])
  expect_equal(result$place.admin.2, c("SHANNAN DIQU", "SKARDU"))
})

test_that("same-country ambiguity is flagged and upstream district repairs are kept", {
  shapes <- afp_overlap_shapes()
  extra <- shapes[2, ]
  extra$GUID <- "{SECOND-CHINA-DISTRICT}"
  extra$ADM2_NAME <- "OTHER DISTRICT"
  shapes <- rbind(shapes, extra)
  cases <- afp_overlap_cases()[1, ]
  result <- afp_overlap_apply(cases, shapes)
  expect_equal(result$geo.review, "ambiguous_district_match")
  expect_true(is.na(result$Admin2GUID))
  expect_equal(result$Admin0GUID, cases$Admin0GUID)
  cases$Admin2GUID <- extra$GUID
  result <- afp_overlap_apply(cases, shapes)
  expect_equal(result$Admin2GUID, extra$GUID)
  expect_equal(result$place.admin.2, "OTHER DISTRICT")
  expect_true(is.na(result$admin2guid))
  expect_true(is.na(result$geo.review))
})

test_that("foreign-only matches and inconsistent source countries retain input geography", {
  cases <- afp_overlap_cases()[1, ]
  shapes <- afp_overlap_shapes()
  sf::st_geometry(shapes)[2] <- sf::st_sfc(afp_overlap_box(10, 12, 10, 12), crs = 4326)
  result <- afp_overlap_apply(cases, shapes)
  expect_equal(result$geo.review, "no_country_spatial_match")
  expect_equal(result$place.admin.0, cases$place.admin.0)
  expect_equal(result$Admin0GUID, cases$Admin0GUID)
  expect_true(is.na(result$Admin2GUID))
  cases$admin0guid <- gsub("[{}]", "", shapes$ADM0_GUID[4])
  cases$Admin0GUID <- shapes$ADM0_GUID[4]
  result <- afp_overlap_apply(cases)
  expect_equal(result$geo.review, "unresolved_source_country")
  expect_equal(result$Admin0GUID, cases$Admin0GUID)
  expect_equal(result$place.admin.0, "CHINA")
})

test_that("district versions respect onset year and duplicate EPIDs keep their rows", {
  shapes <- afp_overlap_shapes()
  future <- shapes[2, ]
  future$GUID <- "{FUTURE-DISTRICT}"
  future$yr.st <- 2026L
  cases <- afp_overlap_cases()[c(1, 1), ]
  result <- afp_overlap_apply(cases, rbind(shapes, shapes[2, ], future))
  expect_equal(nrow(result), 2L)
  expect_equal(result$epid, cases$epid)
  expect_equal(result$Admin2GUID, rep(shapes$GUID[2], 2))
  expect_true(all(is.na(result$geo.review)))
})

test_that("normalization, projected shapes, and missing country GUIDs are supported", {
  cases <- afp_overlap_cases()[1, ]
  cases$admin0guid <- paste0(" {", cases$admin0guid, "} ")
  cases$Admin0GUID <- cases$admin0guid
  result <- afp_overlap_apply(cases, sf::st_transform(afp_overlap_shapes(), 3857))
  expect_equal(result$Admin0GUID, afp_overlap_shapes()$ADM0_GUID[2])
  expect_true(is.na(result$geo.review))
  cases$admin0guid <- cases$Admin0GUID <- NA_character_
  result <- afp_overlap_apply(cases)
  expect_equal(result$Admin0GUID, afp_overlap_shapes()$ADM0_GUID[2])
  expect_true(is.na(result$admin0guid))
})

test_that("missing-coordinate sampling is country/year constrained and preserves every row", {
  withr::local_seed(330)
  cases <- afp_overlap_cases()[c(1, 2, 2, 1), ]
  shapes <- afp_overlap_shapes()
  cases$epid <- paste0("sample-", 1:4)
  cases$polis.latitude <- cases$polis.longitude <- NA_character_
  cases$Admin2GUID <- cases$admin2guid <- c(shapes$GUID[2], shapes$GUID[4], shapes$GUID[3], NA)
  result <- afp_overlap_apply(cases, shapes)
  expect_equal(result$epid, cases$epid)
  expect_true(all(is.finite(result$lat[1:2]) & is.finite(result$lon[1:2])))
  expect_true(all(is.na(result$lat[3:4]) & is.na(result$lon[3:4])))
  expect_equal(result$Admin0GUID, cases$Admin0GUID)
  expect_equal(result$geo.review[3:4], c("no_country_district_match", "missing_district_guid"))
  expect_equal(result$place.admin.2[1:2], c("SHANNAN DIQU", "SKARDU"))
})

test_that("empty inputs return a typed empty result", {
  result <- afp_overlap_apply(afp_overlap_cases()[0, ])
  expect_equal(nrow(result), 0L)
  expect_type(result$geo.review, "character")
  expect_type(result$geo.corrected, "integer")
  expect_type(result$lat, "double")
  expect_type(result$Admin0GUID, "character")
})

test_that("the spatial fallback restores the caller's S2 setting", {
  old_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(TRUE))
  original_join <- sf::st_join
  local_mocked_bindings(st_join = function(...) {
    if (sf::sf_use_s2()) stop("Synthetic S2 geometry failure")
    suppressWarnings(original_join(...))
  }, .package = "sf")
  result <- afp_overlap_apply()
  expect_equal(result$place.admin.0, c("CHINA", "PAKISTAN"))
  expect_true(sf::sf_use_s2())
})

test_that("the final AFP exporter keeps corrected district GUIDs and raw audit values", {
  data <- afp_overlap_apply()
  data$dateonset <- as.Date("2025-01-01")
  for (column in c("datenotify", "dateinvest", "followup.date", "datestool1", "datestool2")) {
    data[[column]] <- as.Date("2025-01-02")
  }
  for (column in c("ontonot", "ontoinvest", "ontostool1", "ontostool2", "stool1tostool2")) {
    data[[column]] <- 1
  }
  data$stool.1.condition <- data$stool.2.condition <- "Good"
  result <- suppressMessages(s2_create_afp_variables(data))
  expect_equal(result$adm0guid, data$Admin0GUID)
  expect_equal(result$adm1guid, data$Admin1GUID)
  expect_equal(result$adm2guid, data$Admin2GUID)
  expect_equal(result$admin2guid, data$admin2guid)
  expect_equal(result$place.admin.0, c("CHINA", "PAKISTAN"))
})

test_that("sampling failures preserve records and restore the geometry engine", {
  old_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(TRUE))
  local_mocked_bindings(st_sample = function(...) stop("Synthetic sampling failure"), .package = "sf")
  cases <- afp_overlap_cases()[1, ]
  cases$Admin2GUID <- afp_overlap_shapes()$GUID[2]
  cases$polis.latitude <- cases$polis.longitude <- NA_character_
  result <- afp_overlap_apply(cases)
  expect_equal(result$epid, cases$epid)
  expect_equal(result$geo.review, "coordinate_sampling_failed")
  expect_true(is.na(result$lat))
  expect_true(sf::sf_use_s2())
})

test_that("a caller using planar geometry keeps that setting", {
  old_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))
  result <- suppressWarnings(afp_overlap_apply())
  expect_equal(result$place.admin.0, c("CHINA", "PAKISTAN"))
  expect_false(sf::sf_use_s2())
})

test_that("crossing polygon edges are repaired without changing the source country", {
  shapes <- afp_overlap_shapes()
  sf::st_geometry(shapes)[2] <- sf::st_sfc(sf::st_polygon(list(matrix(c(
    92, 27, 94, 29, 92, 29, 94, 27, 92, 27
  ), ncol = 2, byrow = TRUE))), crs = 4326)
  cases <- afp_overlap_cases()[1, ]
  cases$polis.longitude <- "92.5"
  cases$polis.latitude <- "28.8"
  result <- suppressWarnings(afp_overlap_apply(cases, shapes))
  expect_equal(result$place.admin.0, "CHINA")
  expect_equal(result$Admin2GUID, shapes$GUID[2])
  expect_true(is.na(result$geo.review))
})
