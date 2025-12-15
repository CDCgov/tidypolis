
library(testthat)
library(dplyr)
library(lubridate)
library(stringr)
library(dplyr)

test_that("format_api_virus_table selects and renames columns correctly", {
  input <- tibble(
    EPID = "123",
    VirusDate = "2021-01-01",
    VirusTypeName = "cVDPV2",
    Admin0Name = "Country",
    Admin1Name = "Region",
    VdpvReportedToHQDate = "2021-01-10",
    VdpvClassificationChangeDate = "2021-01-15"
  )

  result <- format_api_virus_table(input)

  expect_true(all(c("dateonset", "measurement", "place.admin.0", "place.admin.1",
                    "datenotificationtohq", "vdpvclassificationchangedate", "year", "report_date") %in% names(result)))
})

test_that("format_api_virus_table converts date columns to Date type", {
  input <- tibble(
    VirusDate = "2021-01-01",
    VirusTypeName = "VDPV2",
    VdpvReportedToHQDate = "2021-01-10",
    VdpvClassificationChangeDate = "2021-01-15"
  )

  result <- format_api_virus_table(input)

  expect_s3_class(result$dateonset, "Date")
  expect_s3_class(result$datenotificationtohq, "Date")
  expect_s3_class(result$vdpvclassificationchangedate, "Date")
})

test_that("format_api_virus_table assigns correct report_date for VDPV", {
  input <- tibble(
    VirusDate = "2021-01-01",
    VirusTypeName = "VDPV2",
    VdpvClassificationChangeDate = "2021-01-15",
    VdpvReportedToHQDate = NA
  )

  result <- format_api_virus_table(input)

  expect_equal(result$report_date, as.Date("2021-01-15"))
})

test_that("format_api_virus_table assigns correct report_date for WILD1", {
  input <- tibble(
    VirusDate = "2021-01-01",
    VirusTypeName = "WILD1",
    VdpvClassificationChangeDate = NA,
    VdpvReportedToHQDate = "2021-01-10"
  )

  result <- format_api_virus_table(input)

  expect_equal(result$report_date, as.Date("2021-01-10"))
})

test_that("format_api_virus_table formats measurement with space", {
  input <- tibble(
    VirusDate = "2021-01-01",
    VirusTypeName = "VDPV2",
    VdpvClassificationChangeDate = NA,
    VdpvReportedToHQDate = "2021-01-10"
  )

  result <- format_api_virus_table(input)

  expect_equal(result$measurement, "VDPV 2")
})

test_that("format_api_virus_table handles missing optional columns gracefully", {
  input <- tibble(
    VirusDate = "2021-01-01",
    VirusTypeName = "VDPV2",
    VdpvClassificationChangeDate = NA,
    VdpvReportedToHQDate = "2021-01-10"
  )

  result <- format_api_virus_table(input)

  expect_true("measurement" %in% names(result))
  expect_true("dateonset" %in% names(result))
  expect_true("year" %in% names(result))
})

test_that("format_api_virus_table handles NA values in report_date logic", {
  input <- tibble(
    VirusDate = "2021-01-01",
    VirusTypeName = "VDPV2",
    VdpvClassificationChangeDate = NA,
    VdpvReportedToHQDate = "2021-01-10"
  )

  result <- format_api_virus_table(input)

  expect_true(is.na(result$report_date))
})
