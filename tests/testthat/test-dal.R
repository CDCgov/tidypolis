test_that("tidypolis_io stores an RDA object under the requested name", {
  output_file <- tempfile(fileext = ".rda")
  on.exit(unlink(output_file), add = TRUE)
  input <- data.frame(value = 1:3)

  tidypolis_io(
    obj = input,
    io = "write",
    file_path = output_file,
    edav = FALSE,
    object_name = "lab_data"
  )

  output_env <- new.env(parent = emptyenv())
  expect_identical(load(output_file, envir = output_env), "lab_data")
  expect_identical(output_env$lab_data, input)
})
