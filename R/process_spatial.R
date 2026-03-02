#' A function to process WHO spatial datasets
#'
#' @description
#' a function to process WHO spatial datasets
#' @param gdb_folder `str` The folder location of spatial datasets, should end with .gdb,
#' if on edav the gdb will need to be zipped, ensure that the gdb and the zipped file name are the same.
#' @param output_folder `str` Folder location to write outputs to.
#' @param edav `logical` Whether gdb is on EDAV or local.
#' @param azcontainer `Azure container` Azure storage container.
#' @export
#' @examples
#' \dontrun{
#' process_spatial(
#'   gdb_folder = "local_path/GEODATABASE.gdb",
#'   output_folder = "local_path",
#'   edav = F
#' )
#' process_spatial(
#'   gdb_folder = "edav_path/GEODATABASE.gdb",
#'   output_folder = "edav_path",
#'   edav = T
#' )
#' }
process_spatial <- function(gdb_folder = "GID/PEB/SIR/Data/spatial/WHO_POLIO_GLOBAL_GEODATABASE.gdb",
                            output_folder = "GID/PEB/SIR/Data/spatial",
                            edav = TRUE,
                            azcontainer = suppressMessages(sirfunctions::get_azure_storage_connection())) {
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop('Package "utils" must be installed to use this function.',
         .call = FALSE
    )
  }

  if (edav) {
    output_folder <- stringr::str_replace(output_folder, paste0("GID/PEB/SIR/"), "")
  }

  cli::cli_process_start("Loading raw spatial data")

  if (edav) {
    if (!requireNamespace("AzureStor", quietly = TRUE)) {
      stop('Package "AzureStor" must be installed to run process_spatial() on EDAV.',
           .call = FALSE
      )
    }

    withr::with_tempdir({
        AzureStor::storage_multidownload(container = azcontainer,
                                         src = paste0(gdb_folder, "/*"),
                                         dest = "~/geodatabase.gdb",
                                         overwrite = T)

        # Country shapes EDAV===============
        global.ctry.01 <- sf::st_read(dsn = "geodatabase.gdb", layer = "GLOBAL_ADM0")

        # Province shapes EDAV===============
        global.prov.01 <- sf::st_read(dsn = "geodatabase.gdb", layer = "GLOBAL_ADM1")

        # District shapes EDAV===============
        global.dist.01 <- sf::st_read(dsn = "geodatabase.gdb", layer = "GLOBAL_ADM2")
        })

  } else {
    # Country shapes local===============
    global.ctry.01 <- sf::st_read(dsn = gdb_folder, layer = "GLOBAL_ADM0")

    # Province shapes local===============
    global.prov.01 <- sf::st_read(dsn = gdb_folder, layer = "GLOBAL_ADM1")

    # District shapes local===============
    global.dist.01 <- sf::st_read(dsn = gdb_folder, layer = "GLOBAL_ADM2")
  }

  global.ctry.01 <- global.ctry.01 |>
    dplyr::mutate(
      STARTDATE = as.Date(STARTDATE),
      ENDDATE = as.Date(ENDDATE),
      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"), "COTE D IVOIRE", ADM0_NAME)
    )

  global.prov.01 <- global.prov.01 |>
    dplyr::mutate(
      STARTDATE = as.Date(STARTDATE),
      ENDDATE = as.Date(ENDDATE),
      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"), "COTE D IVOIRE", ADM0_NAME)
    )

  global.dist.01 <- global.dist.01 |>
    dplyr::mutate(
      STARTDATE = as.Date(STARTDATE),
      ENDDATE = as.Date(ENDDATE),
      yr.st = lubridate::year(STARTDATE),
      yr.end = lubridate::year(ENDDATE),
      ADM0_NAME = ifelse(stringr::str_detect(ADM0_NAME, "IVOIRE"), "COTE D IVOIRE", ADM0_NAME)
    )

  cli::cli_process_done()

  # identify sf var in global.ctry
  cli::cli_process_start("Country shapefile processing")
  sf_columns_ctry <- sapply(global.ctry.01, function(col) inherits(col, "sfc"))

  sf_var_ctry <- names(global.ctry.01)[sf_columns_ctry]

  cli::cli_process_start("Checking country shapes for validity")
  # identifying bad shapes
  check.ctry.valid <- dplyr::as_tibble(sf::st_is_valid(global.ctry.01))
  row.num.ctry <- which(check.ctry.valid$value == FALSE)
  invalid.ctry.shapes <- global.ctry.01 |>
    dplyr::slice(row.num.ctry) |>
    dplyr::select(ADM0_NAME, GUID, yr.st, yr.end, paste(sf_var_ctry)) |>
    dplyr::arrange(ADM0_NAME)

  sf::st_geometry(invalid.ctry.shapes) <- NULL

  cli::cli_process_done()

  cli::cli_process_start("Outputting invalid country shapes")
  if (edav) {
    tidypolis_io(
      io = "write", edav = T,
      file_path = paste0(output_folder, "/invalid_ctry_shapes.csv"),
      obj = invalid.ctry.shapes, azcontainer = azcontainer
    )
  } else {
    tidypolis_io(
      io = "write", edav = F,
      file_path = paste0(output_folder, "/invalid_ctry_shapes.csv"),
      obj = invalid.ctry.shapes, azcontainer = azcontainer
    )
  }
  cli::cli_process_done()

  empty.ctry <- global.ctry.01 |>
    dplyr::mutate(empty = sf::st_is_empty(!!dplyr::sym(sf_var_ctry))) |>
    dplyr::filter(empty == TRUE)

  sf::st_geometry(empty.ctry) <- NULL

  if (nrow(empty.ctry) > 0) {
    cli::cli_process_start("Outputting empty country shapes")
    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(output_folder, "/empty_ctry_shapes.csv"),
        obj = empty.ctry,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(output_folder, "/empty_ctry_shapes.csv"),
        obj = empty.ctry,
        azcontainer = azcontainer
      )
    }
    cli::cli_process_done()
  }

  rm(invalid.ctry.shapes, check.ctry.valid, row.num.ctry, empty.ctry)

  # identify potential duplicates
  cli::cli_process_start("Checking country shapes for duplicates")
  dupe.guid.ctry <- global.ctry.01 |>
    dplyr::group_by(GUID) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n > 1)

  dupe.name.ctry <- global.ctry.01 |>
    dplyr::group_by(ADM0_NAME, yr.st, yr.end) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n > 1)
  cli::cli_process_done()

  if (nrow(dupe.guid.ctry) > 1 | nrow(dupe.name.ctry) > 1) {
    cli::cli_alert_warning("There is a country shape with an exact duplicate, please manually run shape preprocessing to inspect")
  }

  if (nrow(dupe.guid.ctry) > 1) {
    sf::st_geometry(dupe.guid.ctry) <- NULL
    cli::cli_process_start("Outputting country shapes with duplicate GUIDs")
    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(output_folder, "/duplicate_ctry_guid.csv"),
        obj = dupe.guid.ctry,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(output_folder, "/duplicate_ctry_guid.csv"),
        obj = dupe.guid.ctry,
        azcontainer = azcontainer
      )
    }
  }

  cli::cli_process_done()

  if (nrow(dupe.name.ctry) > 1) {
    sf::st_geometry(dupe.name.ctry) <- NULL
    cli::cli_process_start("Outputting country shapes with duplicate names")
    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(output_folder, "/duplicate_ctry_name.csv"),
        obj = dupe.name.ctry,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(output_folder, "/duplicate_ctry_name.csv"),
        obj = dupe.name.ctry,
        azcontainer = azcontainer
      )
    }
    cli::cli_process_done()
  }

  rm(dupe.guid.ctry, dupe.name.ctry, sf_columns_ctry, sf_var_ctry)

  # ensure CRS of ctry file is 4326
  global.ctry.01 <- sf::st_set_crs(global.ctry.01, 4326)

  # save global country geodatabase in RDS file:
  if (edav) {
    tidypolis_io(
      io = "write", edav = T,
      file_path = paste0(output_folder, "/global.ctry.rds"),
      obj = global.ctry.01,
      azcontainer = azcontainer
    )
  } else {
    tidypolis_io(
      io = "write", edav = F,
      file_path = paste0(output_folder, "/global.ctry.rds"),
      obj = global.ctry.01,
      azcontainer = azcontainer
    )
  }

  sf::st_geometry(global.ctry.01) <- NULL

  cli::cli_process_done()

  # Province shapes overlapping in Lower Juba in Somalia.
  cli::cli_process_start("Province shapefile processing")
  global.prov.01 <- global.prov.01 |>
    dplyr::mutate(yr.end = ifelse(ADM0_GUID == "{B5FF48B9-7282-445C-8CD2-BEFCE4E0BDA7}" &
                                    GUID == "{EE73F3EA-DD35-480F-8FEA-5904274087C4}", 2021, yr.end))

  # identify sf var in global.prov
  sf_columns_prov <- sapply(global.prov.01, function(col) inherits(col, "sfc"))

  sf_var_prov <- names(global.prov.01)[sf_columns_prov]

  cli::cli_process_start("Checking province shape validity")
  check.prov.valid <- dplyr::as_tibble(sf::st_is_valid(global.prov.01))
  row.num.prov <- which(check.prov.valid$value == FALSE)
  invalid.prov.shapes <- global.prov.01 |>
    dplyr::slice(row.num.prov) |>
    dplyr::select(ADM0_NAME, ADM1_NAME, GUID, yr.st, yr.end, paste(sf_var_prov)) |>
    dplyr::arrange(ADM0_NAME)

  sf::st_geometry(invalid.prov.shapes) <- NULL

  cli::cli_process_done()

  cli::cli_process_start("Outputting invalid province shapes")
  if (edav) {
    tidypolis_io(
      io = "write", edav = T,
      file_path = paste0(output_folder, "/invalid_prov_shapes.csv"),
      obj = invalid.prov.shapes,
      azcontainer = azcontainer
    )
  } else {
    tidypolis_io(
      io = "write", edav = F,
      file_path = paste0(output_folder, "/invalid_prov_shapes.csv"),
      obj = invalid.prov.shapes,
      azcontainer = azcontainer
    )
  }
  cli::cli_process_done()

  empty.prov <- global.prov.01 |>
    dplyr::mutate(empty = sf::st_is_empty(!!dplyr::sym(sf_var_prov))) |>
    dplyr::filter(empty == TRUE)

  if (nrow(empty.prov) > 0) {
    cli::cli_process_start("Outputting empty province shapes")
    sf::st_geometry(empty.prov) <- NULL
    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(output_folder, "/empty_prov_shapes.csv"),
        obj = empty.prov,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(output_folder, "/empty_prov_shapes.csv"),
        obj = empty.prov,
        azcontainer = azcontainer
      )
    }
    cli::cli_process_done()
  }

  rm(check.prov.valid, row.num.prov, invalid.prov.shapes, empty.prov)

  # duplicate checking in provinces
  cli::cli_process_start("Checking province shapes for duplicates")
  dupe.guid.prov <- global.prov.01 |>
    dplyr::group_by(GUID) |>
    dplyr::mutate(n = n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n > 1)

  dupe.name.prov <- global.prov.01 |>
    dplyr::group_by(ADM0_NAME, ADM1_NAME, yr.st, yr.end) |>
    dplyr::mutate(n = n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n > 1)

  cli::cli_process_done()
  if (nrow(dupe.guid.prov) > 1 | nrow(dupe.name.prov) > 1) {
    cli::cli_alert_warning("There is a duplicated province that is exactly the same, please run shape preprocessing manually to inspect")
  }

  if (nrow(dupe.guid.prov) > 1) {
    cli::cli_process_start("Outputting provinces with duplicate GUIDs")
    sf::st_geometry(dupe.guid.prov) <- NULL
    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(output_folder, "/duplicate_prov_guid.csv"),
        obj = dupe.guid.prov,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(output_folder, "/duplicate_prov_guid.csv"),
        obj = dupe.guid.prov,
        azcontainer = azcontainer
      )
    }
    cli::cli_process_done()
  }

  if (nrow(dupe.name.prov) > 1) {
    cli::cli_process_start("Outputting provinces with duplicate names")
    sf::st_geometry(dupe.name.prov) <- NULL
    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(output_folder, "/duplicate_prov_name.csv"),
        obj = dupe.name.prov,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(output_folder, "/duplicate_prov_name.csv"),
        obj = dupe.name.prov,
        azcontainer = azcontainer
      )
    }
    cli::cli_process_done()
  }

  rm(dupe.guid.prov, dupe.name.prov, sf_columns_prov, sf_var_prov)

  # ensure CRS is 4326
  global.prov.01 <- sf::st_set_crs(global.prov.01, 4326)
  # save global province geodatabase in RDS file:
  if (edav) {
    tidypolis_io(
      io = "write", edav = T,
      file_path = paste0(output_folder, "/global.prov.rds"),
      obj = global.prov.01,
      azcontainer = azcontainer
    )
  } else {
    tidypolis_io(
      io = "write", edav = F,
      file_path = paste0(output_folder, "/global.prov.rds"),
      obj = global.prov.01,
      azcontainer = azcontainer
    )
  }

  sf::st_geometry(global.prov.01) <- NULL

  cli::cli_process_done()

  # identify sf var in global.dist
  cli::cli_process_start("District shape processing")
  sf_columns_dist <- sapply(global.dist.01, function(col) inherits(col, "sfc"))

  sf_var_dist <- names(global.dist.01)[sf_columns_dist]

  cli::cli_process_start("Checking district shape validity")
  check.dist.valid <- dplyr::as_tibble(sf::st_is_valid(global.dist.01))
  row.num.dist <- which(check.dist.valid$value == FALSE)
  invalid.dist.shapes <- global.dist.01 |>
    dplyr::slice(row.num.dist) |>
    dplyr::select(ADM0_NAME, ADM1_NAME, ADM2_NAME, GUID, yr.st, yr.end, paste(sf_var_dist)) |>
    dplyr::arrange(ADM0_NAME)

  sf::st_geometry(invalid.dist.shapes) <- NULL

  if (edav) {
    tidypolis_io(
      io = "write", edav = T,
      file_path = paste0(output_folder, "/invalid_dist_shapes.csv"),
      obj = invalid.dist.shapes,
      azcontainer = azcontainer
    )
  } else {
    tidypolis_io(
      io = "write", edav = F,
      file_path = paste0(output_folder, "/invalid_dist_shapes.csv"),
      obj = invalid.dist.shapes,
      azcontainer = azcontainer
    )
  }
  cli::cli_process_done()

  empty.dist <- global.dist.01 |>
    dplyr::mutate(empty = sf::st_is_empty(!!dplyr::sym(sf_var_dist))) |>
    dplyr::filter(empty == TRUE)

  if (nrow(empty.dist) > 0) {
    cli::cli_process_start("Outputting empty district shapes")
    sf::st_geometry(empty.dist) <- NULL
    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(output_folder, "/empty_dist_shapes.csv"),
        obj = empty.dist,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(output_folder, "/empty_dist_shapes.csv"),
        obj = empty.dist,
        azcontainer = azcontainer
      )
    }
    cli::cli_process_done
  }

  rm(check.dist.valid, row.num.dist, invalid.dist.shapes, empty.dist)

  # evaluate district duplicates
  cli::cli_process_start("Checking district shapes for duplicates")
  dupe.guid.dist <- global.dist.01 |>
    dplyr::group_by(GUID) |>
    dplyr::mutate(n = n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n > 1)

  dupe.name.dist <- global.dist.01 |>
    dplyr::group_by(ADM0_NAME, ADM1_NAME, ADM2_NAME, yr.st, yr.end) |>
    dplyr::mutate(n = n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n > 1) |>
    dplyr::arrange(ADM0_NAME, ADM1_NAME, ADM2_NAME, yr.st)

  if (nrow(dupe.guid.dist) > 1 | nrow(dupe.name.dist) > 1) {
    cli::cli_alert_warning("There are duplicates in district shapes, please run shape processing manually to inspect")
  }

  if (nrow(dupe.guid.dist) > 1) {
    cli::cli_process_start("Outputting districts with duplicate GUIDs")
    sf::st_geometry(dupe.guid.dist) <- NULL
    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(output_folder, "/duplicate_dist_guid.csv"),
        obj = dupe.guid.dist,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(output_folder, "/duplicate_dist_guid.csv"),
        obj = dupe.guid.dist,
        azcontainer = azcontainer
      )
    }
    cli::cli_process_done()
  }

  if (nrow(dupe.name.dist) > 1) {
    cli::cli_process_start("Outputting districts with duplicate names")
    sf::st_geometry(dupe.name.dist) <- NULL
    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(output_folder, "/duplicate_dist_name.csv"),
        obj = dupe.name.dist,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(output_folder, "/duplicate_dist_name.csv"),
        obj = dupe.name.dist,
        azcontainer = azcontainer
      )
    }
    cli::cli_process_done()
  }

  cli::cli_process_done()
  rm(dupe.guid.dist, dupe.name.dist, sf_columns_dist, sf_var_dist)

  # ensure district CRS is 4326
  global.dist.01 <- sf::st_set_crs(global.dist.01, 4326)
  # save global province geodatabase in RDS file:
  if (edav) {
    tidypolis_io(
      io = "write", edav = T,
      file_path = paste0(output_folder, "/global.dist.rds"),
      obj = global.dist.01,
      azcontainer = azcontainer
    )
  } else {
    tidypolis_io(
      io = "write", edav = F,
      file_path = paste0(output_folder, "/global.dist.rds"),
      obj = global.dist.01,
      azcontainer = azcontainer
    )
  }

  cli::cli_process_done()

  sf::st_geometry(global.dist.01) <- NULL

  endyr <- year(format(Sys.time()))
  startyr <- 2000

  # Province long shape

  df.list <- list()

  for (i in startyr:endyr) {
    df02 <- sirfunctions:::f.yrs.01(global.prov.01, i)

    df.list[[i]] <- df02
  }

  long.global.prov.01 <- dplyr::bind_rows(df.list)
  cli::cli_process_start("Evaluating overlapping province shapes")

  if (endyr == lubridate::year(format(Sys.time())) & startyr == 2000) {
    prov.shape.issue.01 <- long.global.prov.01 |>
      dplyr::group_by(ADM0_NAME, ADM1_NAME, active.year.01) |>
      dplyr::summarise(no.of.shapes = dplyr::n()) |>
      dplyr::filter(no.of.shapes > 1)

    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(
          output_folder, "/prov_shape_multiple_",
          paste(min(prov.shape.issue.01$active.year.01, na.rm = T),
                max(prov.shape.issue.01$active.year.01, na.rm = T),
                sep = "_"
          ), ".csv"
        ),
        obj = prov.shape.issue.01,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(
          output_folder, "/prov_shape_multiple_",
          paste(min(prov.shape.issue.01$active.year.01, na.rm = T),
                max(prov.shape.issue.01$active.year.01, na.rm = T),
                sep = "_"
          ), ".csv"
        ),
        obj = prov.shape.issue.01,
        azcontainer = azcontainer
      )
    }
  }

  cli::cli_process_done()

  # District long shape

  df.list <- list()

  for (i in startyr:endyr) {
    df02 <- sirfunctions:::f.yrs.01(global.dist.01, i)

    df.list[[i]] <- df02
  }

  long.global.dist.01 <- dplyr::bind_rows(df.list)

  cli::cli_process_start("Evaluating overlapping district shapes")

  if (endyr == year(format(Sys.time())) & startyr == 2000) {
    dist.shape.issue.01 <- long.global.dist.01 |>
      dplyr::group_by(ADM0_NAME, ADM1_NAME, ADM2_NAME, active.year.01) |>
      dplyr::summarise(no.of.shapes = dplyr::n()) |>
      dplyr::filter(no.of.shapes > 1)

    if (edav) {
      tidypolis_io(
        io = "write", edav = T,
        file_path = paste0(
          output_folder, "/dist_shape_multiple_",
          paste(min(dist.shape.issue.01$active.year.01, na.rm = T),
                max(dist.shape.issue.01$active.year.01, na.rm = T),
                sep = "_"
          ), ".csv"
        ),
        obj = dist.shape.issue.01,
        azcontainer = azcontainer
      )
    } else {
      tidypolis_io(
        io = "write", edav = F,
        file_path = paste0(
          output_folder, "/dist_shape_multiple_",
          paste(min(dist.shape.issue.01$active.year.01, na.rm = T),
                max(dist.shape.issue.01$active.year.01, na.rm = T),
                sep = "_"
          ), ".csv"
        ),
        obj = dist.shape.issue.01,
        azcontainer = azcontainer
      )
    }
  }

  cli::cli_process_done()
  remove(df.list, df02)
}
