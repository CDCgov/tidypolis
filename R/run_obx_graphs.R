# Load Data Here

# Load function scripts here for now
source(here::here("R/create_sia_graph.R"))



run_obx_sia_graph<- function(obx_table, raw.data, sia_obx_table, run_type, save_graph, cdc_upload){


if(run_type == "all"){

  for (i in 1:nrow(obx_table)){
    x <- obx_table$ob_id[i]
    df_sub <- obx_table |> dplyr::filter(ob_id == x)

    g1 <- gen_obx_map_reg_sia(df_sub, raw.data, sia_obx_table, x)


 if (save_graph == "yes"){
   fn1 = paste(df_sub$ob_country,"_", df_sub$ob_id,"_", raw.data$metadata$download_time,".png", sep="")

   temp_path <- file.path(tempdir(), fn1)

   # Save Locally
   ggplot2::ggsave(temp_path, plot= g1, height = 6.2, width = 14, unit = "in", dpi = 300)

   if (cdc_upload == "yes"){
     # Set Sharepoint -> for writing to sharepoint
     sp_path <- paste("./OBX/SitAwr/obx_test/")

     # Create new folder
     sp_newfolder <- paste("obx_sia_all", as.character(raw.data$metadata$download_time), sep = "_")
     sp_newpath <- paste(sp_path, sp_newfolder, "/", sep="")
     sp_newpath_full <- paste(sp_newpath, fn1, sep="")

     # Upload to Sharepoint
     sirfunctions::upload_to_sharepoint(
       file_to_upload = temp_path,
       sharepoint_file_loc = sp_newpath_full,
       site = "https://cdc.sharepoint.com/teams/CGH-GID-PEB",
       drive = "Documents")

     cli::cli_alert(paste(fn1, "uploaded to Sharepoint", sep = " "))

   }else{
     cli::cli_alert(paste(fn1, "temp copy only", sep = " "))
   }

 }else{
  cli::cli_alert("No filed saved locally")
 }
}

}else if (run_type == "active_ob"){
  obx_active <- obx_table |>
                   dplyr::filter(ob_status_bin == "active")

  for (i in 1:nrow(obx_active)){
    x <- obx_active$ob_id[i]
    df_sub <- obx_active |> dplyr::filter(ob_id == x)

    g1 <- gen_obx_map_reg_sia(df_sub, raw.data, sia_obx_table, x)


    if (save_graph == "yes"){
      fn1 = paste(df_sub$ob_country,"_", df_sub$ob_id,"_", raw.data$metadata$download_time,".png", sep="")

      temp_path <- file.path(tempdir(), fn1)

      # Save Locally
      ggplot2::ggsave(temp_path, plot= g1, height = 6.2, width = 14, unit = "in", dpi = 300)

      if (cdc_upload == "yes"){
        # Set Sharepoint -> for writing to sharepoint
        sp_path <- paste("./OBX/SitAwr/obx_test/")

        # Create new folder
        sp_newfolder <- paste("obx_sia_all", as.character(raw.data$metadata$download_time), sep = "_")
        sp_newpath <- paste(sp_path, sp_newfolder, "/", sep="")
        sp_newpath_full <- paste(sp_newpath, fn1, sep="")

        # Upload to Sharepoint
        sirfunctions::upload_to_sharepoint(
          file_to_upload = temp_path,
          sharepoint_file_loc = sp_newpath_full,
          site = "https://cdc.sharepoint.com/teams/CGH-GID-PEB",
          drive = "Documents")

        cli::cli_alert(paste(fn1, "uploaded to Sharepoint", sep = " "))

      }else{
        cli::cli_alert(paste(fn1, "temp copy only", sep = " "))
      }

    }else{
      cli::cli_alert("No filed saved locally")
    }


}}else{
  cli::cli_alert("Check run_type code - no graphs produced")
}
}



