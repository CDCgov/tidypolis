gen_obx_sia_br_rds <- function(positives.clean.01,sia_sub, sia_sub2,sia_fun, df_sub){
  # X<- "ANG-cVDPV2-1"
  # df_sub <- obx_table |> dplyr::filter(ob_id == x)


# Need to look at code here in first loop to ensure it goes back for areas that are not convered with other SIAs to
# Not exclude

  base <- df_sub |>
    dplyr::ungroup() |>
    dplyr::select(ob_id, ob_country, ob_srt_admin1, ipv_ctry, ob_srt_epid, ob_srt_onset, ob_srt_d0, first_reg_sia,first_reg_sia_end,
                  sec_reg_sia,sec_reg_sia_end, most_recent, int_brk_vr, sia_date_upper)

  # Local inputs to extract
  ob_start <- base |> tail(1) |> dplyr::pull(ob_srt_d0)
  ob_end <-   base |> tail(1) |> dplyr::pull(most_recent)
  ob_sia_end <- base |> tail(1) |> dplyr::pull(sia_date_upper)
  # ob_type <- base |> tail(1) |> dplyr::pull(int_brk_vr)
  siabr_start <- base |> tail(1) |> dplyr::pull(sec_reg_sia)
  siabr_end <-  base |> tail(1) |> dplyr::pull(sec_reg_sia_end)
  ctry       <-  base |> tail(1) |> dplyr::pull(ob_country)

  sia1_start <- base |> tail(1) |> dplyr::pull(first_reg_sia)
  sia2_start <- base |> tail(1) |> dplyr::pull(sec_reg_sia)

  base <- base |> dplyr::select(-sia_date_upper)

  # Was the initial SIA response covering the first region?
  # Pull ADM1 of regions covered
  # Check with SK tomorrow if we filter to all or ones with two
  sia_info <- sia_sub |>
    dplyr::filter(
      place.admin.0 == ctry &
        activity.start.date == sia1_start |
        activity.start.date == sia2_start )

  sia_prov_covered <-sia_info |>
    dplyr::distinct(place.admin.1) |>
    dplyr::pull(place.admin.1)


  # Pull in remaining SIAs
  sia_count <- sia_sub |>
    dplyr::filter(activity.start.date > sia2_start) |>
    dplyr::distinct(sia.code, activity.start.date) |>
    dplyr::arrange(activity.start.date)




  base <- base |>
    dplyr::mutate(indx_ob_reg_cov = ifelse(ob_srt_admin1 %in% sia_prov_covered, "y", "n"),
                  sia_rds_remain = nrow(sia_count))



  # Check with Steph here if it should be the end plus 28 or start plus 28 + 3
  all_ob_viruses <- positives.clean.01 |>
    dplyr::filter(
      place.admin.0 == ctry &
        measurement == sero &
        dateonset >= df_sub$fv_onset &
        dateonset <= df_sub$most_recent)
  # Next Virus
  all_ob_viruses <- all_ob_viruses |>
    dplyr::filter(!(place.admin.1 %in% sia_prov_covered &
                      dateonset < (siabr_end + lubridate::days(28))))

  base <- base |>
    dplyr::mutate(virus_remain = nrow(all_ob_viruses))


  if ((base |> tail(1) |> dplyr::pull(virus_remain) != 0) &
       !(base |> tail(1) |> dplyr::pull(int_brk_vr) %in% c("3_<2Rds", "4_newob", "5_ipvctry"))){


    next_virus <- all_ob_viruses |>
      dplyr::arrange(report_date, dateonset) |>
      dplyr::select(epid, place.admin.0, place.admin.1, dateonset, report_date) |>
      dplyr::summarise(
        ob_id = x,
        ob_country = dplyr::first(place.admin.0),
        ob_srt_admin1 = dplyr::first(place.admin.1),
        ob_srt_epid = dplyr::first(epid),
        ob_srt_onset = dplyr::first(dateonset),
        ob_srt_d0 = dplyr::first(report_date))

    next_virus1 <- positives.clean.01 |>
      dplyr::filter(
        place.admin.0 == ctry &
          measurement == sero &
          dateonset > (siabr_end + lubridate::days(28)) &
          dateonset <= ob_end) |>
      dplyr::arrange(dateonset) |>
      dplyr::select(dateonset) |>
      dplyr::summarise(
        ob_id = x,
        # first_onset = dplyr::first(dateonset),
        most_recent = dplyr::last(dateonset))

    next_virus <- dplyr::left_join(next_virus, next_virus1, by = "ob_id")
    sia_next <- next_virus |> dplyr::pull(ob_srt_d0)
    adm1_sia_next <- next_virus |> dplyr::pull(ob_srt_admin1)


    next_sia_code_data <- sia_fun |>
              dplyr::filter(place.admin.0 == ctry &
                     place.admin.1 == adm1_sia_next &
                     activity.start.date >sia_next)


    if(nrow(next_sia_code_data) != 0){
      next_sia_code <- next_sia_code_data |>
              dplyr::arrange(activity.start.date) |>
        dplyr::group_by(activity.start.date, sia.code) |>
        dplyr::count() |>
              head(1) |>
              dplyr::pull(sia.code)

     next_sia <- sia_sub |>
      dplyr::filter(sia.code ==  next_sia_code) |>
      dplyr::summarise(
        sia_name = dplyr::first(sia.code),
        first_reg_sia = dplyr::first(lubridate::as_date(activity.start.date)),
        first_reg_sia_end = dplyr::first(lubridate::as_date(activity.end.date)))

    if (is.na(next_sia$sia_name)==T){
      cli::cli_alert(paste("Can't find SIA activity code - checking in main dataset"))
      next_sia <- sia_sub |>
        dplyr::filter(next_sia_code == sia.code) |>
        dplyr::summarise(
          sia_name = dplyr::first(sia.code),
          first_reg_sia = lubridate::as_date(dplyr::first(activity.start.date)),
          first_reg_sia_end = lubridate::as_date(dplyr::first(activity.end.date)))
      if (is.na(next_sia$sia_name)==F){
        cli::cli_alert(paste("Found - Moving On"))
      }else{
        cli::cli_abort("SIA Should be Present - Check OB function")
      }

    }else{}


    next_virus <- dplyr::bind_cols(next_virus, next_sia)
    b1 <- base |> tail(1) |>
      dplyr::select(ob_id, ipv_ctry)

    next_virus <- dplyr::left_join(next_virus, b1, by = "ob_id") |>
      dplyr::relocate(ipv_ctry, .after = ob_country)

    next_sia_code <- next_virus |> dplyr::pull(sia_name)
    next_sia_str_date <- next_virus |> dplyr::pull(first_reg_sia)


    sia_count <- sia_count |>
      dplyr::filter(!sia.code == next_sia_code)


    }else{ # select the next SIA ## Will need to fix code here once it shows up with no viruses
      next_sia <- dplyr::tibble(
        sia_name = NA_character_,
        first_reg_sia = lubridate::as_date(NA),
        first_reg_sia_end = lubridate::as_date(NA)
        )

      next_virus <- dplyr::bind_cols(next_virus, next_sia)
      b1 <- base |> tail(1) |>
        dplyr::select(ob_id, ipv_ctry)

      next_virus <- dplyr::left_join(next_virus, b1, by = "ob_id") |>
        dplyr::relocate(ipv_ctry, .after = ob_country)


      next_sia_code_data <- sia_fun

      # Remaining SIAS
      # next_sia_code <- next_virus |> dplyr::pull(sia_name)
      #
      sia_count <- sia_count
      # |> dplyr::filter(!sia.code == next_sia_code)

    }


sia_complete <- next_virus |> dplyr::pull(sia_name)
# Pulls out all SIAs that have been covered previous
# may need to be updated here to another if_else variable
# If virus has not been covered setting timeframe for one year of
if(is.na(sia_complete)==F){

    sia1_start <- next_virus |> tail(1) |> dplyr::pull(first_reg_sia)
    sia1_end <- next_virus |> tail(1) |> dplyr::pull(first_reg_sia_end)
    sia1_name <- next_virus |> tail(1) |> dplyr::pull(sia_name)

    # Regions covered by SIA - what S

    # Create check if there were any SIAs done before the flagged SIA &
    # proceeding regions

    if(any(sia_count$activity.start.date < sia1_start)){
      #Code needs to check each of the SIA and keep in any viruses that are covered by SIAS
      # before proceeding round - keep those viruses in


      sia_check <- sia_count |>
         dplyr::filter(activity.start.date < sia1_start)

      # Insert Cli Alter here

      virus_check_list <- list()

      for (i in 1:nrow(sia_check)){
        sd <- sia_check$activity.start.date[i]
        id_name <- sia_check$sia.code[i]

      #Filter to admins
      sia_checK_admins <- sia_fun |>
                           dplyr::filter(activity.start.date == sd &
                                         sia.code == id_name) |>
                           dplyr::distinct(place.admin.1) |>
                            dplyr::pull(place.admin.1)


      # Are there any viruses that were responded too first
      virus_check <- all_ob_viruses |>
                      dplyr::filter(
                        dateonset < sd &
                        place.admin.1 %in% sia_checK_admins &
                        report_date < sd)
      # &report_date < sd

      if(nrow(virus_check)==0){

        sia_count <- sia_count |>
      dplyr::filter(!(activity.start.date == sd &
                      sia.code == id_name))

      cli::cli_alert(paste0("SIA:", sia_check$sia.code[i], " removed - did not cover additional viruses"))

      }else{}

      virus_check_list[[i]] <- virus_check
}
      # Combine all viruses to keep
      virus_check_list <- do.call(plyr::rbind.fill, virus_check_list)

      virus_check_list <- virus_check_list|>
         dplyr::distinct(epid, .keep_all =T)


      sia_info <- sia_fun |>
        dplyr::filter(
          place.admin.0 == ctry &
            activity.start.date == sia1_start &
            sia.code == sia1_name)

      sia_prov_covered <-sia_info |>
        dplyr::distinct(place.admin.1) |>
        dplyr::pull(place.admin.1)

# Change here to include SIA start
      all_ob_viruses <- all_ob_viruses |>
        dplyr::filter(!(place.admin.1 %in% sia_prov_covered &
                          dateonset < (sia1_end + lubridate::days(28))))

      all_ob_viruses <- dplyr::bind_rows(all_ob_viruses, virus_check_list)

      all_ob_viruses <- all_ob_viruses |> dplyr::distinct(epid, .keep_all = T )


      cli::cli_alert(paste0("Additional SIAs before currt Rd found, added ", nrow(virus_check_list), " viruses back in"))

    }else{
      # If no viruses - cleares all by the first campaign

    sia_info <- sia_fun |>
      dplyr::filter(
        place.admin.0 == ctry &
          activity.start.date == sia1_start &
          sia.code == sia1_name)

    sia_prov_covered <-sia_info |>
      dplyr::distinct(place.admin.1) |>
      dplyr::pull(place.admin.1)

    sia_count <- sia_count |>
      dplyr::filter(!(activity.start.date == sia1_start &
                      sia.code == sia1_name))

  # Change here to include report date back in

    all_ob_viruses <- all_ob_viruses |>
      dplyr::filter(!(place.admin.1 %in% sia_prov_covered &
                        dateonset < (sia1_end + lubridate::days(28))))

    # &report_date < sia1_start

    cli::cli_alert(paste0("Removed all covered viruses from matched SIA"))

}
}else{
  all_ob_viruses <- all_ob_viruses |>
    dplyr::filter(!(place.admin.1 == adm1_sia_next &
                  report_date <= (next_virus$ob_srt_d0 + lubridate::years(1))))
  cli::cli_alert(paste0("No SIA found, removed all viruses in ADMIN 1 Region for next year"))

}


# sia_count <- sia_count |>
#   dplyr::filter(!(activity.start.date == sia1_start))
# Remove from SIA FUN to prevent loop
if(is.na(sia_complete)==F){

    sia_fun <- sia_fun |>
              dplyr::filter(!(activity.start.date == sia1_start &
                              sia.code == sia1_name))
}else{}


    next_virus <-    next_virus |>
      dplyr::mutate(int_brk_vr = dplyr::case_when(
        is.na(ipv_ctry) == T  & most_recent > (first_reg_sia_end + lubridate::days(28)) ~ "1_y",
        is.na(ipv_ctry) == T  & most_recent <= (first_reg_sia_end + lubridate::days(28)) ~ "2_n",
        is.na(ipv_ctry) == F ~ "5_ipvctry",
        is.na(first_reg_sia_end) == T ~ "6_nosia"))|>
      dplyr::relocate(most_recent, .after = first_reg_sia_end) |>
      dplyr::mutate(indx_ob_reg_cov = ifelse(ob_srt_admin1 %in% sia_prov_covered, "y", "n"),
                    virus_remain = nrow(all_ob_viruses),
                    sia_rds_remain = nrow(sia_count))



    base <- dplyr::bind_rows(base, next_virus)





    # Add in loop to identify each subsequent SIA that should occur due to breakthrough response
 while((base |> tail(1) |> dplyr::pull(virus_remain) != 0)){

      # Local inputs to extract
      ob_start <- base |> tail(1) |> dplyr::pull(ob_srt_d0)
      ob_end <-   base |> tail(1) |> dplyr::pull(most_recent)

# If no SIA here go back to start and update the SIA information or for code above keep pulling down previous SIA
      if (is.na(base |> tail(1) |> dplyr::pull(first_reg_sia))==T &
          is.na(base |> tail(1) |> dplyr::pull(first_reg_sia_end))==T){

        siabr_start <- base |> head(1) |> dplyr::pull(sec_reg_sia)
        siabr_end <-  base |> head(1) |> dplyr::pull(sec_reg_sia_end)
      }else{
        siabr_start <- base |> tail(1) |> dplyr::pull(first_reg_sia)
        siabr_end <-  base |> tail(1) |> dplyr::pull(first_reg_sia_end)
      }



      next_virus <- all_ob_viruses |>
        dplyr::arrange(report_date, dateonset) |>
        dplyr::select(epid, place.admin.0, place.admin.1, dateonset, report_date) |>
        dplyr::summarise(
          ob_id = x,
          ob_country = dplyr::first(place.admin.0),
          ob_srt_admin1 = dplyr::first(place.admin.1),
          ob_srt_epid = dplyr::first(epid),
          ob_srt_onset = dplyr::first(dateonset),
          ob_srt_d0 = dplyr::first(report_date))

      next_virus1 <- positives.clean.01 |>
        dplyr::filter(
          place.admin.0 == ctry &
            measurement == sero &
            dateonset <= ob_end) |>
        dplyr::arrange(dateonset) |>
        dplyr::select(dateonset) |>
        dplyr::summarise(
          ob_id = x,
          most_recent = dplyr::last(dateonset))

      next_virus <- dplyr::left_join(next_virus, next_virus1, by = "ob_id")

      sia_next <- next_virus |> dplyr::pull(ob_srt_d0)
      adm1_sia_next <- next_virus |> dplyr::pull(ob_srt_admin1)


      next_sia_code_data <- sia_fun |>
        dplyr::filter(place.admin.0 == ctry &
                        place.admin.1 == adm1_sia_next &
                        activity.start.date >sia_next)

      if(nrow(next_sia_code_data) != 0){
        next_sia_code <- next_sia_code_data |>
          dplyr::arrange(activity.start.date) |>
          dplyr::group_by(activity.start.date, sia.code) |>
          dplyr::count() |>
          head(1) |>
          dplyr::pull(sia.code)

        next_sia <- sia_sub |>
          dplyr::filter(sia.code ==  next_sia_code) |>
          dplyr::summarise(
            sia_name = dplyr::first(sia.code),
            first_reg_sia = dplyr::first(lubridate::as_date(activity.start.date)),
            first_reg_sia_end = dplyr::first(lubridate::as_date(activity.end.date)))

        if (is.na(next_sia$sia_name)==T){
          cli::cli_alert(paste("Can't find SIA activity code - checking in main dataset"))
          next_sia <- sia_sub |>
            dplyr::filter(next_sia_code == sia.code) |>
            dplyr::summarise(
              sia_name = dplyr::first(sia.code),
              first_reg_sia = lubridate::as_date(dplyr::first(activity.start.date)),
              first_reg_sia_end = lubridate::as_date(dplyr::first(activity.end.date)))
          if (is.na(next_sia$sia_name)==F){
            cli::cli_alert(paste("Found - Moving On"))
          }else{
            cli::cli_abort("SIA Should be Present - Check OB function")
          }

        }else{}


        next_virus <- dplyr::bind_cols(next_virus, next_sia)

        # sia_count <- sia_count |>
        #             dplyr::filter(activity.start.date >= next_sia_code)

      }else{ # select the next SIA ## Will need to fix code here once it shows up with no viruses

        # Bring in SIA Data
        next_sia <- dplyr::tibble(
            sia_name = NA_character_,
            first_reg_sia = lubridate::as_date(NA),
            first_reg_sia_end = lubridate::as_date(NA))

        next_virus <- dplyr::bind_cols(next_virus, next_sia)


        next_sia_code_data <- sia_fun
        sia_count <- sia_count
      }



############################################################################
      sia_complete <- next_virus |> dplyr::pull(sia_name)
      # Pulls out all SIAs that have been covered previous
      # may need to be updated here to another if_else variable
      # If virus has not been covered setting timeframe for one year of
      if(is.na(sia_complete)==F){

        sia1_start <- next_virus |> tail(1) |> dplyr::pull(first_reg_sia)
        sia1_end <- next_virus |> tail(1) |> dplyr::pull(first_reg_sia_end)
        sia1_name <- next_virus |> tail(1) |> dplyr::pull(sia_name)



        # Create check if there were any SIAs done before the flagged SIA &
        # proceeding regions

        if(any(sia_count$activity.start.date < sia1_start)){
          #Code needs to check each of the SIA and keep in any viruses that are covered by SIAS
          # before proceeding round - keep those viruses in


          sia_check <- sia_count |>
            dplyr::filter(activity.start.date < sia1_start)

          # Insert Cli Alter here

          virus_check_list <- list()

          for (i in 1:nrow(sia_check)){
            sd <- sia_check$activity.start.date[i]

            #Filter to admins
            sia_checK_admins <- sia_fun |>
              dplyr::filter(activity.start.date == sd &
                              sia.code == id_name) |>
              dplyr::distinct(place.admin.1) |>
              dplyr::pull(place.admin.1)


            # Are there any viruses that were responded too first
            virus_check <- all_ob_viruses |>
              dplyr::filter(
                dateonset < sd &
                  place.admin.1 %in% sia_checK_admins &
                  report_date < sd)

            # &
            #   report_date < sd

            if(nrow(virus_check)==0){

              sia_count <- sia_count |>
                dplyr::filter(!(activity.start.date == sd &
                                sia.code == id_name))

              cli::cli_alert(paste0("SIA:", sia_check$sia.code[i], " removed - did not cover additional viruses"))

            }else{}

            virus_check_list[[i]] <- virus_check
          }
          # Combine all viruses to keep
          virus_check_list <- do.call(plyr::rbind.fill, virus_check_list)

          virus_check_list <- virus_check_list|>
            dplyr::distinct(epid, .keep_all =T)

          sia_info <- sia_sub |>
            dplyr::filter(
              place.admin.0 == ctry &
                activity.start.date == sia1_start)

          sia_prov_covered <-sia_info |>
            dplyr::distinct(place.admin.1) |>
            dplyr::pull(place.admin.1)

          all_ob_viruses <- all_ob_viruses |>
            dplyr::filter(!(place.admin.1 %in% sia_prov_covered &
                              dateonset < (sia1_end + lubridate::days(28))))

          # &report_date < sia1_start

          all_ob_viruses <- dplyr::bind_rows(all_ob_viruses, virus_check_list)

          cli::cli_alert(paste0("Additional SIAs before currt Rd found, added ", nrow(virus_check_list), " viruses back in"))

        }else{
          # If no viruses - cleares all by the first campaign

          sia_info <- sia_sub |>
            dplyr::filter(
              place.admin.0 == ctry &
                activity.start.date == sia1_start)

          sia_prov_covered <-sia_info |>
            dplyr::distinct(place.admin.1) |>
            dplyr::pull(place.admin.1)

          sia_count <- sia_count |>
            dplyr::filter(!(activity.start.date == sia1_start &
                              sia.code == sia1_name))


          all_ob_viruses <- all_ob_viruses |>
            dplyr::filter(!(place.admin.1 %in% sia_prov_covered &
                              dateonset < (sia1_end + lubridate::days(28))))

          # &report_date < sia1_start

          cli::cli_alert(paste0("Removed all covered viruses from matched SIA"))

        }
      }else{
        all_ob_viruses <- all_ob_viruses |>
          dplyr::filter(!(place.admin.1 == adm1_sia_next &
                            report_date <= (next_virus$ob_srt_d0 + lubridate::years(1))))
        cli::cli_alert(paste0("No SIA found, removed all viruses in ADMIN 1 Region for next year"))

      }

      b1 <- base |> tail(1) |>
        dplyr::select(ob_id, ipv_ctry)

      next_virus <- dplyr::left_join(next_virus, b1, by = "ob_id") |>
        dplyr::relocate(ipv_ctry, .after = ob_country)


      sia_count <- sia_count |>
        dplyr::filter(!(activity.start.date == sia1_start &
                          sia.code == sia1_name))


      if(is.na(sia_complete)==F){

        sia_fun <- sia_fun |>
          dplyr::filter(!(activity.start.date == sia1_start &
                            sia.code == sia1_name))
      }else{}


      next_virus <-    next_virus |>
        dplyr::mutate(int_brk_vr = dplyr::case_when(
          is.na(ipv_ctry) == T  & most_recent > (first_reg_sia_end + lubridate::days(28)) ~ "1_y",
          is.na(ipv_ctry) == T  & most_recent <= (first_reg_sia_end + lubridate::days(28)) ~ "2_n",
          is.na(ipv_ctry) == F ~ "5_ipvctry",
          is.na(first_reg_sia_end) == T ~ "6_nosia"))|>
        dplyr::relocate(most_recent, .after = first_reg_sia_end) |>
        dplyr::mutate(indx_ob_reg_cov = ifelse(ob_srt_admin1 %in% sia_prov_covered, "y", "n"),
                      virus_remain = nrow(all_ob_viruses),
                      sia_rds_remain = nrow(sia_count))



      base <- dplyr::bind_rows(base, next_virus)
 }

    br_sub <- base
    br_sub <- br_sub|>
      dplyr::mutate(subid = dplyr::row_number(),
                    ob_sia_id = paste0(ob_id, "-", subid),
                    br_viz = dplyr::case_when(
                      dplyr::row_number() == 1 ~ sec_reg_sia,
                      TRUE ~ first_reg_sia)
                    ) |>

      dplyr::select(-subid, -sec_reg_sia, -sec_reg_sia_end)
return(br_sub)



# }else if (base |> tail(1) |> dplyr::pull(virus_remain) != 0 &
#           !(base |> tail(1) |> dplyr::pull(int_brk_vr) %in% c("3_<2Rds",  "4_newob", "5_ipvctry"))){
#   base <- df_sub |>
#     dplyr::ungroup() |>
#     dplyr::select(ob_id, ob_country, ob_srt_admin1, ipv_ctry, ob_srt_epid, ob_srt_onset, ob_srt_d0, first_reg_sia,first_reg_sia_end,
#                   sec_reg_sia,sec_reg_sia_end, most_recent, int_brk_vr, sia_date_upper)
#
#
#   # Local inputs to extract
#   ob_start <- base |> tail(1) |> dplyr::pull(ob_srt_d0)
#   ob_end <-   base |> tail(1) |> dplyr::pull(most_recent)
#   ob_sia_end <- base |> tail(1) |> dplyr::pull(sia_date_upper)
#   siabr_start <- base |> tail(1) |> dplyr::pull(sec_reg_sia)
#   siabr_end <-  base |> tail(1) |> dplyr::pull(sec_reg_sia_end)
#   ctry       <-  base |> tail(1) |> dplyr::pull(ob_country)
#
#   sia1_start <- base |> tail(1) |> dplyr::pull(first_reg_sia)
#   sia2_start <- base |> tail(1) |> dplyr::pull(sec_reg_sia)
#
#   base <- base |> dplyr::select(-sia_date_upper)
#
#   # Was the initial SIA response covering the first region?
#   # Pull ADM1 of regions covered
#   # Check with SK tomorrow if we filter to all or ones with two
#   sia_info <- sia_sub |>
#     dplyr::filter(
#       place.admin.0 == ctry &
#         activity.start.date == sia1_start |
#         activity.start.date == sia2_start )
#
#   sia_prov_covered <-sia_info |>
#     dplyr::distinct(place.admin.1) |>
#     dplyr::pull(place.admin.1)
#
#
#   # Pull in remaining SIAs
#   sia_count <- sia_sub |>
#     dplyr::filter(activity.start.date > sia2_start) |>
#     dplyr::distinct(sia.code, activity.start.date)
#
#   base <- base |>
#     dplyr::mutate(indx_ob_reg_cov = ifelse(ob_srt_admin1 %in% sia_prov_covered, "y", "n"),
#                   sia_rds_remain = nrow(sia_count))
#
#
#
#   # Check with Steph here if it should be the end plus 28 or start plus 28 + 3
#   all_ob_viruses <- positives.clean.01 |>
#     dplyr::filter(
#       place.admin.0 == ctry &
#         measurement == sero &
#         dateonset >= df_sub$fv_onset &
#         dateonset <= df_sub$most_recent)
#
#   # Next Virus
#   all_ob_viruses <- all_ob_viruses |>
#     dplyr::filter(!(place.admin.1 %in% sia_prov_covered &
#                       dateonset < (siabr_end + lubridate::days(28))))
#
#   base <- base |>
#     dplyr::mutate(virus_remain = nrow(all_ob_viruses))
#
#
#   next_virus <- all_ob_viruses |>
#     dplyr::arrange(report_date, dateonset) |>
#     dplyr::select(epid, place.admin.0, place.admin.1, dateonset, report_date) |>
#     dplyr::summarise(
#       ob_id = x,
#       ob_country = dplyr::first(place.admin.0),
#       ob_srt_admin1 = dplyr::first(place.admin.1),
#       ob_srt_epid = dplyr::first(epid),
#       ob_srt_onset = dplyr::first(dateonset),
#       ob_srt_d0 = dplyr::first(report_date))
#
#   next_virus1 <- positives.clean.01 |>
#     dplyr::filter(
#       place.admin.0 == ctry &
#         measurement == sero &
#         dateonset > (siabr_end + lubridate::days(28)) &
#         dateonset <= ob_end) |>
#     dplyr::arrange(dateonset) |>
#     dplyr::select(dateonset) |>
#     dplyr::summarise(
#       ob_id = x,
#       # first_onset = dplyr::first(dateonset),
#       most_recent = dplyr::last(dateonset))
#
#
#   next_virus <- dplyr::left_join(next_virus, next_virus1, by = "ob_id")
#   sia_next <- next_virus |> dplyr::pull(ob_srt_d0)
#   adm1_sia_next <- next_virus |> dplyr::pull(ob_srt_admin1)
#
#
#   next_sia_code_data <- sia_fun |>
#     dplyr::filter(place.admin.0 == ctry &
#                     place.admin.1 == adm1_sia_next &
#                     activity.start.date >sia_next)
#
#
#   if(nrow(next_sia_code_data) != 0){
#     next_sia_code <- next_sia_code_data |>
#       dplyr::arrange(activity.start.date) |>
#       dplyr::group_by(sia.code, activity.start.date) |>
#       dplyr::count() |>
#       head(1) |>
#       dplyr::pull(sia.code)
#
#     next_sia <- sia_sub2 |>
#       dplyr::filter(sia_code ==  next_sia_code) |>
#       dplyr::summarise(
#         sia_name = dplyr::first(sia_code),
#         first_reg_sia = dplyr::first(sia_date),
#         first_reg_sia_end = dplyr::first(sia_date_end))
#
#     next_virus <- dplyr::bind_cols(next_virus, next_sia)
#
#   }else{ # select the next SIA ## Will need to fix code here once it shows up with no viruses
#     next_sia <- dplyr::tibble(
#       sia_name = NA_character_,
#       first_reg_sia = lubridate::as_date(NA),
#       first_reg_sia_end = lubridate::as_date(NA))
#
#     next_virus <- dplyr::bind_cols(next_virus, next_sia)
#
#   }
#
#
#   b1 <- base |> tail(1) |>
#     dplyr::select(ob_id, ipv_ctry)
#
#   next_virus <- dplyr::left_join(next_virus, b1, by = "ob_id") |>
#     dplyr::relocate(ipv_ctry, .after = ob_country)
#
#
#   # Remaining SIAS
#   next_sia_code <- next_virus |> dplyr::pull(sia_name)
#
#   sia_count <- sia_count |>
#     dplyr::filter(!sia.code == next_sia_code)
#
#   if(nrow(next_sia_code_data) != 0){
#
#     sia1_start <- next_virus |> tail(1) |> dplyr::pull(first_reg_sia)
#
#     sia_info <- sia_data |>
#       dplyr::filter(
#         place.admin.0 == ctry &
#           activity.start.date == sia1_start)
#
#     sia_prov_covered <-sia_info |>
#       dplyr::distinct(place.admin.1) |>
#       dplyr::pull(place.admin.1)
#
#     all_ob_viruses <- all_ob_viruses |>
#       dplyr::filter(!(place.admin.1 %in% sia_prov_covered &
#                         dateonset < (siabr_end + lubridate::days(28))))
#
#   }else{
#     all_ob_viruses <- all_ob_viruses |>
#       dplyr::filter(!place.admin.1 == adm1_sia_next)
#
#   }
#
#
#   next_virus <-    next_virus |>
#     dplyr::mutate(int_brk_vr = dplyr::case_when(
#       is.na(ipv_ctry) == T  & most_recent > (first_reg_sia_end + lubridate::days(28)) ~ "1_y",
#       is.na(ipv_ctry) == T  & most_recent <= (first_reg_sia_end + lubridate::days(28)) ~ "2_n",
#       is.na(ipv_ctry) == F ~ "5_ipvctry",
#       is.na(first_reg_sia_end) == T ~ "6_nosia"))|>
#     dplyr::relocate(most_recent, .after = first_reg_sia_end) |>
#     dplyr::mutate(indx_ob_reg_cov = ifelse(ob_srt_admin1 %in% sia_prov_covered, "y", "n"),
#                   virus_remain = nrow(all_ob_viruses),
#                   sia_rds_remain = nrow(sia_count))
#
#
#   base <- dplyr::bind_rows(base, next_virus)
#
#   br_sub <- base
#   br_sub <- br_sub|>
#     dplyr::mutate(subid = dplyr::row_number(),
#                   ob_sia_id = paste0(ob_id, "-", subid),
#                   br_viz = dplyr::case_when(
#                     dplyr::row_number() == 1 ~ sec_reg_sia,
#                     TRUE ~ first_reg_sia)) |>
#     dplyr::select(-subid, -sec_reg_sia, -sec_reg_sia_end)
#
#     return(br_sub)
#
}else{

    br_sub <- base
    br_sub <- br_sub|>
      dplyr::mutate(subid = dplyr::row_number(),
                    ob_sia_id = paste0(ob_id, "-", subid),
                    br_viz = dplyr::case_when(
                      dplyr::row_number() == 1 ~ sec_reg_sia,
                      TRUE ~ first_reg_sia)) |>
      dplyr::select(-subid, -sec_reg_sia, -sec_reg_sia_end)
return(br_sub)

}
}


##############################################################################


# Code for part 1 above if complete failure
# sia_complete <- next_virus |> dplyr::pull(sia_name)




# if(is.na(next_virus |> tail(1) |> dplyr::pull(sia_name)==T)){
#
#     all_ob_viruses <- all_ob_viruses |>
#       dplyr::filter(!(place.admin.1 == adm1_sia_next &
#                      report_date <= (next_virus$ob_srt_d0 + lubridate::years(1))))
#
#     cli::cli_alert(paste0("No SIA found, removed all viruses in ADMIN 1 Region for next year"))
#
#
# }else{
#    # Remaining SIAS
#     next_sia_code <- next_virus |> dplyr::pull(sia_name)
#
#     next_sia_str_date <- next_virus |> dplyr::pull(first_reg_sia)
#
#
#     sia_count <- sia_count |>
#       dplyr::filter(!(sia.code == next_sia_code))
#
#     # sia_count <- sia_count |>
#     #   dplyr::filter(!(activity.start.date < next_sia_str_date))
# }
#
#
# if(nrow(next_sia_code_data) != 0){
#         siabr_end <- next_virus |> dplyr::pull(first_reg_sia_end)
#
#         sia1_start <- next_virus |>  dplyr::pull(first_reg_sia)
#
#
#         # Create check if there were any SIAs done before the flagged SIA &
#         # proceeding regions
#
#         if(any(sia_count$activity.start.date < sia1_start)){
#           #Code needs to check each of the SIA and keep in any viruses that are covered by SIAS
#           # before proceeding round - keep those viruses in
#
#
#           sia_check <- sia_count |>
#             dplyr::filter(activity.start.date < sia1_start)
#
#           # Insert Cli Alter here
#
#           virus_check_list <- list()
#
#           for (i in 1:nrow(sia_check)){
#             sd <- sia_check$activity.start.date[i]
#
#             #Filter to admins
#             sia_checK_admins <- sia_fun |>
#               dplyr::filter(activity.start.date == sd) |>
#               dplyr::distinct(place.admin.1) |>
#               dplyr::pull(place.admin.1)
#
#
#             # Are there any viruses that were responded too first
#             virus_check <- all_ob_viruses |>
#               dplyr::filter(
#                 dateonset <= sd &
#                   place.admin.1 %in% sia_checK_admins &
#                   report_date <= sd)
#
#             if(nrow(virus_check)==0){
#
#               sia_count <- sia_count |>
#                 dplyr::filter(!(activity.start.date == sd))
#
#               cli::cli_alert(paste0("SIA:", sia_check$sia.code[i], " removed - did not cover additional viruses"))
#
#             }else{}
#
#             virus_check_list[[i]] <- virus_check
#           }
#           # Combine all viruses to keep
#           virus_check_list <- do.call(plyr::rbind.fill, virus_check_list)
#
#         sia_info <- sia_data |>
#           dplyr::filter(
#             place.admin.0 == ctry &
#               activity.start.date == sia1_start)
#
#         sia_prov_covered <-sia_info |>
#           dplyr::distinct(place.admin.1) |>
#           dplyr::pull(place.admin.1)
#
#         all_ob_viruses <- all_ob_viruses |>
#           dplyr::filter(!(place.admin.1 %in% sia_prov_covered &
#                             dateonset < (siabr_end + lubridate::days(28))))
#
#         all_ob_viruses <- dplyr::bind_rows(all_ob_viruses, virus_check_list)
#
#
#         next_virus <- next_virus |>
#           dplyr::mutate(indx_ob_reg_cov = ifelse(ob_srt_admin1 %in% sia_prov_covered, "y", "n"))
#
#
#         cli::cli_alert(paste0("Additional SIAs before currt Rd found, added ", nrow(virus_check_list), " viruses back in"))
#
#       }else{
#
#         sia_info <- sia_data |>
#           dplyr::filter(
#             place.admin.0 == ctry &
#               activity.start.date == sia1_start)
#
#         sia_prov_covered <-sia_info |>
#           dplyr::distinct(place.admin.1) |>
#           dplyr::pull(place.admin.1)
#
#         all_ob_viruses <- all_ob_viruses |>
#           dplyr::filter(!(place.admin.1 %in% sia_prov_covered &
#                             dateonset < (sia1_end + lubridate::days(28))))
#
#         cli::cli_alert(paste0("No additional SIAs found - removed all covered viruses"))
#
# }else{
#
#
#         if(nrow(sia_count)==0){
#           all_ob_viruses <- all_ob_viruses |>
#             dplyr::filter(!(place.admin.1 == adm1_sia_next &
#                               report_date <= (next_virus$ob_srt_d0 + lubridate::years(1))))
#         }else{
#           # all_ob_viruses <- all_ob_viruses |>
#           #   dplyr::filter(!(place.admin.1 == adm1_sia_next &
#           #                     report_date <= (next_virus$ob_srt_d0 + lubridate::years(1))))
#         }
#
#         next_virus <- next_virus |>
#           dplyr::mutate(indx_ob_reg_cov = "no_sia")
#
#
#   }
# }
#
#       next_virus <- dplyr::left_join(next_virus, b1, by = "ob_id") |>
#         dplyr::relocate(ipv_ctry, .after = ob_country)
#
#       next_virus <-    next_virus |>
#         dplyr::mutate(int_brk_vr = dplyr::case_when(
#           is.na(ipv_ctry) == T  & most_recent > (first_reg_sia_end + lubridate::days(28)) ~ "1_y",
#           is.na(ipv_ctry) == T  & most_recent <= (first_reg_sia_end + lubridate::days(28)) ~ "2_n",
#           is.na(ipv_ctry) == F ~ "5_ipvctry",
#           is.na(first_reg_sia_end) == T ~ "6_nosia"))|>
#         dplyr::relocate(most_recent, .after = first_reg_sia_end) |>
#         dplyr::mutate(virus_remain = nrow(all_ob_viruses),
#                       sia_rds_remain = nrow(sia_count))
#
#
#        base <- dplyr::bind_rows(base, next_virus)
#  }

