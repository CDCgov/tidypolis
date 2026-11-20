# Run from the repository root:
# testthat::test_file("Sandbox/test_fix_spatial_country_metadata.R")
testthat::skip_if_not_installed("sf")
script <- if (file.exists("Sandbox/fix_spatial_country_metadata.R")) {
  "Sandbox/fix_spatial_country_metadata.R"
} else "fix_spatial_country_metadata.R"
source(script, local = TRUE)

country_test_shapes <- function() {
  sf::st_sf(
    ADM0_NAME = c("COTE D IVOIRE", "C\u00f4te d'Ivoire", "GUINEA-BISSAU", "GUINEA BISSAU", "GUINEA", NA),
    WHO_REGION = c(NA, "", "AFRO", "AFRO", "AFRO", NA),
    GUID = c("ivory1", "ivory2", "discard", "keep", "guinea", "unknown"),
    STARTDATE = as.Date(rep("2000-01-01", 6)),
    ENDDATE = as.Date(rep("9999-12-31", 6)),
    Shape = sf::st_sfc(lapply(seq_len(6), function(i) sf::st_point(c(i, i))), crs = 4326)
  )
}

testthat::test_that("corrections keep the preferred Guinea Bissau geometry and other metadata", {
  original <- country_test_shapes()
  result <- fix_spatial_country_metadata(original)
  expected <- original[-3, ]
  expected$WHO_REGION[1:2] <- "AFRO"
  testthat::expect_identical(result$shapes, expected)
  testthat::expect_identical(original$WHO_REGION[1:2], c(NA_character_, ""))
  testthat::expect_equal(result$summary$guinea_bissau_rows_removed, 1)
  testthat::expect_equal(result$summary$ivoire_regions_fixed, 2)
  testthat::expect_setequal(result$changes$GUID, c("ivory1", "ivory2", "discard"))
  testthat::expect_identical(fix_spatial_country_metadata(result$shapes)$shapes, result$shapes)
})

testthat::test_that("the only Guinea Bissau representation cannot be removed", {
  testthat::expect_error(fix_spatial_country_metadata(country_test_shapes()[-4, ]), "without GUINEA BISSAU")
})

testthat::test_that("factor regions accept AFRO and empty layers are preserved", {
  x <- country_test_shapes()[1:2, ]
  x$WHO_REGION <- factor(c(NA, ""))
  testthat::expect_equal(as.character(fix_spatial_country_metadata(x)$shapes$WHO_REGION), rep("AFRO", 2))
  empty <- x[FALSE, ]
  testthat::expect_identical(fix_spatial_country_metadata(empty)$shapes, empty)
})

testthat::test_that("Timor Leste and North Macedonia variants get the requested regions", {
  names <- c("Timor-Leste", " TIMOR LESTE ", "NORTH MACEDONIA",
             "REPUBLIC OF NORTH MACEDONIA", "EPUBLIC OF NORTH MACEDONIA", "OTHER")
  x <- country_test_shapes()
  x$ADM0_NAME <- names
  x$WHO_REGION <- c(NA, "", "AFRO", NA, "", "WPRO")
  result <- fix_spatial_country_metadata(x)
  expected <- x
  expected$WHO_REGION <- c("SEARO", "SEARO", "EURO", "EURO", "EURO", "WPRO")
  testthat::expect_identical(result$shapes, expected)
  testthat::expect_equal(result$summary$timor_leste_regions_fixed, 2)
  testthat::expect_equal(result$summary$north_macedonia_regions_fixed, 3)
  testthat::expect_equal(result$summary$ivoire_regions_fixed, 0)
  testthat::expect_identical(result$changes$new_WHO_REGION, expected$WHO_REGION[1:5])
  testthat::expect_identical(result$changes$action, paste0("assign_", expected$WHO_REGION[1:5]))
  repeated <- fix_spatial_country_metadata(result$shapes)
  testthat::expect_identical(repeated$shapes, result$shapes)
  testthat::expect_equal(nrow(repeated$changes), 0)

  # Region assignment also works when a layer stores WHO_REGION as a factor.
  x$WHO_REGION <- factor(x$WHO_REGION)
  testthat::expect_identical(as.character(fix_spatial_country_metadata(x)$shapes$WHO_REGION), expected$WHO_REGION)
})

testthat::test_that("both folders are staged, verified, and backed up without changing sources", {
  root <- tempfile("spatial-fixes-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  sources <- c(Data_spatial = file.path(root, "Data", "spatial"), POLIS_misc = file.path(root, "POLIS", "misc"))
  for (folder in sources) {
    dir.create(folder, recursive = TRUE)
    for (level in c("ctry", "prov", "dist")) {
      x <- country_test_shapes()
      x$ADM0_NAME[5:6] <- c("TIMOR-LESTE", "EPUBLIC OF NORTH MACEDONIA")
      x$WHO_REGION[5:6] <- NA_character_
      if (level != "ctry") x$ADM0_GUID <- x$GUID
      saveRDS(x, file.path(folder, paste0("global.", level, ".rds")))
    }
  }
  output <- file.path(root, "corrected")
  result <- run_spatial_country_fixes(sources, edav = FALSE, output_dir = output)
  testthat::expect_equal(nrow(result$summary), 6)
  testthat::expect_true(all(result$summary$timor_leste_regions_fixed == 1))
  testthat::expect_true(all(result$summary$north_macedonia_regions_fixed == 1))
  for (copy in names(sources)) for (level in c("ctry", "prov", "dist")) {
    filename <- paste0("global.", level, ".rds")
    original <- readRDS(file.path(sources[[copy]], filename))
    testthat::expect_identical(readRDS(file.path(output, copy, "originals", filename)), original)
    testthat::expect_identical(readRDS(file.path(output, copy, filename)), fix_spatial_country_metadata(original)$shapes)
    saved <- readRDS(file.path(output, copy, filename))
    testthat::expect_identical(saved$WHO_REGION[saved$ADM0_NAME %in% "TIMOR-LESTE"], "SEARO")
    testthat::expect_identical(saved$WHO_REGION[saved$ADM0_NAME %in% "EPUBLIC OF NORTH MACEDONIA"], "EURO")
    testthat::expect_true("GUINEA-BISSAU" %in% original$ADM0_NAME)
  }
  testthat::expect_true(file.exists(file.path(output, "summary.csv")))
  testthat::expect_error(run_spatial_country_fixes(sources, edav = FALSE, output_dir = output), "new output_dir")
})

testthat::test_that("a removed country cannot leave retained child records orphaned", {
  original <- country_test_shapes()
  corrected <- fix_spatial_country_metadata(original)$shapes
  child <- corrected
  child$ADM0_GUID <- c("ivory1", "ivory2", "{DISCARD}", "guinea", "unknown")
  testthat::expect_error(.check_spatial_country_links(list(ctry = original),
    list(ctry = corrected, prov = child, dist = child)), "removed country GUID")
})
