gen_obx_sia_br_rds <- function(positives.clean.01,sia_sub2,df_sub){
  # x <- "ANG-cVDPV2-1"
  # df_sub <- obx_table |> dplyr::filter(ob_id == x)

  if (df_sub |> tail(1) |> dplyr::pull(int_brk_vr) == "1_y"){

    base <- df_sub |>
      dplyr::ungroup() |>
      dplyr::select(ob_id, ob_country, ipv_ctry, ob_srt_epid, ob_srt_onset, ob_srt_d0, first_reg_sia,first_reg_sia_end,
                    sec_reg_sia,sec_reg_sia_end, most_recent, int_brk_vr, sia_date_upper)


    # Local inputs to extract
    ob_start <- base |> tail(1) |> dplyr::pull(ob_srt_d0)
    ob_end <-   base |> tail(1) |> dplyr::pull(most_recent)
    ob_sia_end <- base |> tail(1) |> dplyr::pull(sia_date_upper)
    siabr_start <- base |> tail(1) |> dplyr::pull(sec_reg_sia)
    siabr_end <-  base |> tail(1) |> dplyr::pull(sec_reg_sia_end)
    ctry       <-  base |> tail(1) |> dplyr::pull(ob_country)

    base <- base |> dplyr::select(-sec_reg_sia,-sec_reg_sia_end, -sia_date_upper)


    next_virus <- positives.clean.01 |>
      dplyr::filter(
        place.admin.0 == ctry &
          measurement == sero &
          dateonset > (siabr_end + lubridate::days(28)) &
          dateonset <= ob_end) |>
      dplyr::arrange(report_date, dateonset) |>
      dplyr::select(epid, place.admin.0, place.admin.1, dateonset, report_date) |>
      dplyr::summarise(
        ob_id = x,
        ob_country = dplyr::first(place.admin.0),
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

    next_sia <- sia_sub2 |>
      dplyr::filter(sia_date >= sia_next &
                    sia_date < ob_sia_end) |>
      dplyr::summarise(
        first_reg_sia = dplyr::first(sia_date),
        first_reg_sia_end = dplyr::first(sia_date_end))

    next_virus <- dplyr::bind_cols(next_virus, next_sia)

    b1 <- base |>
      dplyr::select(ob_id, ipv_ctry)

    next_virus <- dplyr::left_join(next_virus, b1, by = "ob_id") |>
      dplyr::relocate(ipv_ctry, .after = ob_country)
    next_virus <-    next_virus |>
      dplyr::mutate(int_brk_vr = dplyr::case_when(
        is.na(ipv_ctry) == T  & most_recent > (first_reg_sia_end + lubridate::days(28)) ~ "1_y",
        is.na(ipv_ctry) == T  & most_recent <= (first_reg_sia_end + lubridate::days(28)) ~ "2_n",
        is.na(ipv_ctry) == F ~ "5_ipvctry",
        is.na(first_reg_sia_end) == T ~ "6_nosia"))|>
      dplyr::relocate(most_recent, .after = first_reg_sia_end)

    base <- dplyr::bind_rows(base, next_virus)

    # Add in loop to identify each subsequent SIA that should occur due to breakthrough response
    while(base |> tail(1) |> dplyr::pull(int_brk_vr) == "1_y"){

      # Local inputs to extract
      ob_start <- base |> tail(1) |> dplyr::pull(ob_srt_d0)
      ob_end <-   base |> tail(1) |> dplyr::pull(most_recent)
      siabr_start <- base |> tail(1) |> dplyr::pull(first_reg_sia)
      siabr_end <-  base |> tail(1) |> dplyr::pull(first_reg_sia_end)

      #Identify  the first breakthrough virus was after the breakthrough
      next_virus <- positives.clean.01 |>
        dplyr::filter(
          place.admin.0 == ctry &
            measurement == sero &
            dateonset > (siabr_end + lubridate::days(28)) &
            dateonset <= ob_end) |>
        dplyr::arrange(report_date, dateonset) |>
        dplyr::select(epid, place.admin.0, place.admin.1, dateonset, report_date) |>
        dplyr::summarise(
          ob_id = x,
          ob_country = dplyr::first(place.admin.0),
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
          most_recent = dplyr::last(dateonset))

      # Gets around reporting lags here:
      next_virus <- dplyr::left_join(next_virus, next_virus1, by = "ob_id")


      # Pull out next virus start data - even if there is a round inbetween -
      # Country wouldn't have know virus had spread while planning it and reacting to the first "true"
      # breakthrough virus - need to capture all true rounds in master obx table and how they differ

      sia_next <- next_virus$ob_srt_d0


      # Bring in SIA Data
      next_sia <- sia_sub2 |>
        dplyr::filter(sia_date >= sia_next) |>
        dplyr::summarise(
          first_reg_sia = dplyr::first(sia_date),
          first_reg_sia_end = dplyr::first(sia_date_end))

      next_virus <- dplyr::bind_cols(next_virus, next_sia)

      b1 <- base |>
        dplyr::select(ob_id, ipv_ctry) |>
        dplyr::distinct(ob_id, .keep_all=T)

      next_virus <- dplyr::left_join(next_virus, b1, by = "ob_id") |>
        dplyr::relocate(ipv_ctry, .after = ob_country)

      next_virus <-    next_virus |>
        dplyr::mutate(int_brk_vr = dplyr::case_when(
          is.na(ipv_ctry) == T  & most_recent > (first_reg_sia + lubridate::days(28)) ~ "1_y",
          is.na(ipv_ctry) == T  & most_recent <= (first_reg_sia_end + lubridate::days(28)) ~ "2_n",
          is.na(ipv_ctry) == F ~ "5_ipvctry",
          is.na(first_reg_sia_end) == T ~ "6_nosia"))|>
        dplyr::relocate(most_recent, .after = first_reg_sia_end)

      base <- dplyr::bind_rows(base, next_virus)
    }

    br_sub <- base
    br_sub <- br_sub|>
      dplyr::mutate(subid = dplyr::row_number(),
                    ob_sia_id = paste0(ob_id, "-", subid)) |>
      dplyr::select(-subid)
return(br_sub)

}else{
    base <- df_sub |>
    dplyr::ungroup() |>
    dplyr::select(ob_id, ob_country, ipv_ctry, ob_srt_epid, ob_srt_onset, ob_srt_d0, first_reg_sia,first_reg_sia_end, most_recent, int_brk_vr)

    br_sub <- base
    br_sub <- br_sub|>
      dplyr::mutate(subid = dplyr::row_number(),
                    ob_sia_id = paste0(ob_id, "-", subid)) |>
      dplyr::select(-subid)
return(br_sub)

  }

}
