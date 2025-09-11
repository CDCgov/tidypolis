# Generate map that shows all detections with all SIAs detections to ensure that the detecitons and SIA windows line
# Will move over to SIR functions after

# Look into Chad's outbreak
# Because of extended SIAs - anchored on the first date - 3 plus 28 to get the breakthrough window



gen_obx_map_reg_sia<- function(df_sub, raw.data, sia_obx_table, sia_full_all, viz, x){

ob_start <- lubridate::as_date(df_sub$fv_onset - lubridate::dmonths(1))

sia1_start <- df_sub |> dplyr::pull(first_reg_sia)
sia1_end   <- df_sub |> dplyr::pull(first_reg_sia_end)
sia2_start <- df_sub |> dplyr::pull(sec_reg_sia)
sia2_end <- df_sub |> dplyr::pull(sec_reg_sia_end)

ob_sia_start <- df_sub$ob_srt_d0
ob_end <- df_sub$sia_date_upper
ctry <- df_sub$ob_country
sero <- df_sub$ob_type

ob_min <- ob_start

if(df_sub$ob_status == "1_active_u6mos"){
  ob_max <- lubridate::today()
}else{
ob_max  <- lubridate::as_date(df_sub$most_recent + lubridate::dmonths(12))
}


if(sero == "cVDPV 1"){
  virus <- c("cVDPV 1", "VDPV 1")
}else if(sero == "cVDPV 2"){
  virus <- c("cVDPV 2", "VDPV 2")
}else if(sero == "cVDPV 3"){
  virus <- c("cVDPV 3", "VDPV 3")
}else{}

# SIA Data
sia_phases <- sia_obx_table |> dplyr::filter(ob_id == x) |>
                dplyr::mutate(br_viz=dplyr::if_else(is.na(br_viz)==T, df_sub$date_upper, br_viz),
                              br_sub = dplyr::if_else( int_brk_vr == "6_nosia", NA, br_viz))


v_map <- raw.data[["pos"]] |>
  dplyr::filter(dateonset >=ob_start & dateonset < ob_end &
                measurement %in% virus &
                place.admin.0 == ctry) |>
  dplyr::arrange(place.admin.0, measurement, dateonset) |>
  dplyr::select(place.admin.0, place.admin.1, place.admin.2, adm0guid, adm1guid, admin2guid, epid, measurement, dateonset, emergencegroup,
                is.breakthrough, source, report_date, admin0whocode) |>
  dplyr::rename(adm2guid = admin2guid) |>
  dplyr::distinct(epid, measurement, .keep_all = T) |>
  dplyr::mutate(surv = dplyr::case_when(
    source == "AFP" ~ "AFP",
    source == "ENV" ~ "ES",
    source %in% c("Community", "Contact",
                  "Healthy", "Other", "iVDPV") ~ "OTHER"),
    emergencegroup = dplyr::case_when(emergencegroup =
                                       measurement == "VDPV 1" ~ "VDPV 1",
                                       measurement == "VDPV 2" ~ "VDPV 2",
                                       measurement == "VDPV 3" ~ "VDPV 3",
                                       TRUE ~ emergencegroup),
    sias = dplyr::case_when(
      measurement == "cVDPV 2" |
        measurement == "VACCINE 2" |
        measurement == "VACCINE 2-n" ~ list(c("mOPV2", "nOPV2", "tOPV")),
      measurement == "cVDPV 1" |
        measurement == "WILD 1" ~ list(c("bOPV", "tOPV", "mOPV1")),
      measurement == "cVDPV 3" |
        measurement == "WILD 3"  ~ list(c("bOPV", "tOPV", "mOPV3")))) |>
  dplyr::arrange(place.admin.1) |>
  dplyr::mutate(place.admin.1 = factor(place.admin.1, levels=unique(place.admin.1)))


# Pull Day0 Breakthrough Infections
day0_data <- sia_obx_table |>
  dplyr::filter(ob_id == x) |>
  dplyr::mutate(day0_viz = ifelse(dplyr::row_number()==1, "Init", "BrkRp"))



# Pull in SIA Data
# For now just done responses
region_virus <- unique(v_map$adm1guid)
sia_1 <- unique(unlist(v_map$sias))

# Fill in missing SIA data data
sia_data_sub <- raw.data$sia |>
  dplyr::filter( place.admin.0 %in% ctry &
                   adm1guid %in% region_virus &
                   vaccine.type %in% sia_1 &
                   status %in% c("Done")) |>
  dplyr::mutate(proxy_date_flag = dplyr::case_when(
    is.na(sub.activity.end.date)==T ~ "proxy_enddate",
    is.na(sub.activity.start.date)==T ~ "proxy_srt_date"),
    sub.activity.end.date = dplyr::if_else(is.na(sub.activity.end.date)==T, (sub.activity.start.date + lubridate::days(3)), sub.activity.end.date))

if(x == "MAA-cVDPV2-1"){
  sia_data_sub <- raw.data$sia |>
    dplyr::filter( place.admin.0 %in% ctry &
                     vaccine.type %in% sia_1 &
                     status %in% c("Done")) |>
    dplyr::mutate(proxy_date_flag = dplyr::case_when(
      is.na(sub.activity.end.date)==T ~ "proxy_enddate",
      is.na(sub.activity.start.date)==T ~ "proxy_srt_date"),
      sub.activity.end.date = dplyr::if_else(is.na(sub.activity.end.date)==T, (sub.activity.start.date + lubridate::days(3)), sub.activity.end.date))

}else{}

sia_data_sub <- sia_data_sub|>
  dplyr::filter(
             sub.activity.end.date >= ob_sia_start &
             sub.activity.end.date <= ob_end ) |>
  dplyr::mutate(activity.start.date = lubridate::ymd(activity.start.date),
         activity.end.date = lubridate::ymd(activity.end.date),
         sia_proxy = "SIA",
         status = factor(status)) |>
  dplyr::arrange(activity.start.date, sia.code, place.admin.0, place.admin.1) |>
  dplyr::distinct(sia.code, place.admin.1, .keep_all = TRUE) |>
  dplyr::mutate(place.admin.1 = dplyr::if_else(place.admin.1 == "NAIROBI COUNTY", "NAIROBI", place.admin.1))


if(nrow(sia_data_sub) == 0){
sia_data_sub <- sia_data_sub |>
    dplyr::mutate(
      sia_rd_viz = NA)
}else{
sia_codes <- sia_full_all |>
                dplyr::select(sia_date, sia_country, sia_rd_viz)

sia_data_sub <- dplyr::left_join(sia_data_sub, sia_codes, by = c("activity.start.date" = "sia_date",
                                                                   "place.admin.0" ="sia_country")) |>
                dplyr::mutate(
                  sia_rd_viz = factor(sia_rd_viz, levels = c(
                    "round_0","first_sia", "second_sia",
                    "mopup_sia", "brk_sia", "extra_sias")),
                  sia_brk_viz = dplyr::case_when(
                    sia_rd_viz %in% c("second_sia", "brk_sia") ~ sub.activity.end.date + lubridate::days(28)))


}

# Create window for IPV countries of new outbreaks
if(df_sub$int_brk_vr %in% c("3_<2Rds", "4_newob", "5_ipvctry")){

download_date <- raw.data$metadata$processed_time
virus_ul <- df_sub$most_recent + lubridate::dmonths(12)
t_date <- lubridate::today()
upper_limit <- dplyr::if_else(download_date >= virus_ul,  virus_ul, lubridate::as_date(t_date))

}else{

}


g1 <- ggplot2::ggplot() +
    ggplot2::geom_point(data=v_map, ggplot2::aes(x=place.admin.1, y = dateonset, shape = measurement, color = emergencegroup),  size = 3) +
    ggplot2::scale_color_manual(name = "Emergence Group(s):", values = emg_cols, guide = ggplot2::guide_legend(order = 2, nrow = 4)) +
    ggnewscale::new_scale_color()  +
    ggplot2::geom_point(data=day0_data, ggplot2::aes(x=ob_srt_admin1, y = ob_srt_onset, color = day0_viz), shape=1,  size = 6, stroke = 1) +
    ggplot2::scale_color_manual(name = "Emergence Group(s):", values = emg_cols) +
    ggplot2::scale_color_manual(name = "OBX/Event Response:",
                               values = c(
                                 "Init" = "black",
                                 "BrkRp" = "#CD2626"),
                               labels = c(
                                 "Init" = "Initial",
                                 "BrkRp" = "Expanded"),
                               guide = ggplot2::guide_legend(order = 3, ncol = 1, override.aes = list(size=3))) +
    ggplot2::coord_flip() +
    ggplot2::xlab("Impacted Region(s)") +
    ggplot2::ylab("Date of Onset or Collection / SIA Activity Start Date") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_date(date_breaks = "4 months",
                          date_labels = "%b '%y",
                          limits = c(lubridate::as_date(ob_min), lubridate::as_date(ob_max))) +
    ggnewscale::new_scale_color()  +
  ggplot2::geom_errorbar(data=sia_phases,
                         ggplot2::aes(x=ob_srt_admin1,
                                      y = lubridate::as_date(ob_srt_d0),
                                      ymin=lubridate::as_date(ob_srt_d0),,
                                      ymax = lubridate::as_date(ob_srt_d0)),
                         width = 1.2,
                         linetype = "dotted")

 if(viz == "sen_check"){
  g1 <- g1+  ggplot2::geom_errorbar(data=sia_data_sub,
                           ggplot2::aes(x=place.admin.1,
                                        y = activity.start.date,
                                        ymin=activity.start.date,
                                        ymax = activity.start.date,
                                        color = sia_rd_viz),
                           width = 0.75,
                           linetype = "solid")

 }else{
   g1 <- g1 + ggplot2::geom_errorbar(data=sia_data_sub,
                         ggplot2::aes(x=place.admin.1,
                                      y = activity.start.date,
                                      ymin=activity.start.date,
                                      ymax = activity.start.date,
                                      linetype = status),
                         width = 0.75,
                         show.legend = F)
 }
g1 <- g1 +  ggplot2::scale_shape_manual(name = "Detection:",
                                values = c(
                                  "VDPV 1" =  8,
                                  "VDPV 2" =  8,
                                  "VDPV 3" =  8,
                                  "cVDPV 2" = 16,
                                  "cVDPV 1" = 16,
                                  "cVDPV 3" = 16,
                                  "WILD 1" = 15),
                                labels = c( "VDPV 1" =  "VDPV1",
                                            "VDPV 2" =  "VDPV2",
                                            "VDPV 3" =  "VDPV3",
                                            "cVDPV 2" = "cVDPV2",
                                            "cVDPV 1" = "cVDPV1",
                                            "cVDPV 3" = "cVDPV3",
                                            "WILD 1" = "WPV1"),
                                breaks = c("cVDPV 1", "cVDPV 2", "cVDPV 3",
                                           "VDPV 1", "VDPV 2", "VDPV 3", "WILD 1"),
                                guide = ggplot2::guide_legend(order = 1, ncol = 1),drop = F) +
    ggplot2::scale_color_manual(name = "SIA:",
                                values =  c("round_0" = "green",
                                            "first_sia" ="lightblue",
                                            "second_sia" = "dodgerblue",
                                            "mopup_sia" = "orange",
                                            "brk_sia"= "#CD2626",
                                            "extra_sias" = "black"),
                                labels =  c("round_0" = "Rd0 (Proxy)",
                                            "first_sia" ="Rd1",
                                            "second_sia" = "Rd2",
                                            "mopup_sia" = "Rd:Mop-Up",
                                            "brk_sia"= "Rd: Breakthrough",
                                        "extra_sias" = "Rd: Addtl/Planned"),
                                guide = ggplot2::guide_legend(order = 4, ncol = 2))
if(df_sub$int_brk_vr %in% c("3_<2Rds", "4_newob", "5_ipvctry")){

  g1 <- g1 +  ggplot2::geom_rect(data = sia_phases,
                       ggplot2::aes(xmin = -Inf,
                                    xmax = Inf,
                                    ymin = lubridate::as_date(ob_srt_d0), ymax = lubridate::as_date(upper_limit)), fill = "goldenrod2", alpha = .2)

}else{
  g1 <- g1 +  ggplot2::geom_linerange(data = sia_phases,
                           ggplot2::aes(x = ob_srt_admin1,
                                        ymin = lubridate::as_date(ob_srt_d0), ymax = lubridate::as_date(br_viz) + lubridate::days(3)), color = "grey40", alpha = .2, size = 5) +
        ggplot2::geom_linerange(data = sia_data_sub,
                       ggplot2::aes(x = place.admin.1 ,ymin = lubridate::as_date(sub.activity.end.date), ymax = lubridate::as_date(sia_brk_viz)), color = "blue", alpha = .2, size = 5) +

        ggplot2::geom_linerange(data = sia_phases,
                            ggplot2::aes(x = ob_srt_admin1, ymin = lubridate::as_date(ob_srt_onset), ymax = lubridate::as_date(ob_srt_d0)), color = "#EE7942", alpha = .2, size = 5)
}
g1 <-  g1 +
    # ggplot2::geom_hline(yintercept = lubridate::as_date(sia_phases$ob_srt_d0), color = "grey30", linetype = "dotted", linewidth = 0.9) +
    ggplot2::geom_hline(yintercept =  lubridate::as_date(ob_sia_start), color = "grey30", linetype = "dotted", linewidth = 0.9) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      axis.text =  ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.key = ggplot2::element_blank(),
      legend.title.position = "top",
      legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 10),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(size = 12,
                                         face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10)) +
    # ggplot2::guides(
    #   color = ggplot2::guide_legend(order = 3, nrow = 1),
    #   shape = ggplot2::guide_legend(order = 1, override.aes = list(size=3), ncol=1)) +
    ggplot2::labs(
      title = paste0(stringr::str_to_title(df_sub$ob_country), ": ", stringr::str_replace_all(df_sub$ob_type, " ", ""), " Outbreak (", df_sub$ob_id, ")"),
      subtitle = paste0("Status: ", df_sub$ob_status_bin))

if(df_sub$int_brk_vr %in% c("3_<2Rds", "4_newob", "5_ipvctry")){
  g1 <- g1 + ggplot2::labs(caption = paste0("Circles repersent detections that trigger initial outbreak or expanded outbreak responses.\n",
                                            "Yellow shading indicates an IPV-only using country, a newly reported event or incomplete initial SIA response.\n",
                                            "Produced by: CDC-CGH-GID-PEB. Source: POLIS (Data as of ",raw.data$metadata$download_time,")", sep= ""))
}else{
      g1 <- g1 + ggplot2::labs(caption = paste0("Circles repersent detections that trigger initial outbreak or expanded outbreak responses.\n",
                       "Orange shading indicates time from onset/collectiont to day 0 of expanded response.\n",
                       "Grey shading indicates time from day 0 to the start date of an SIA response.\n",
                       "Blue shading indicates 28-day post SIA washout period.\n",
                       "Produced by: CDC-CGH-GID-PEB. Source: POLIS (Data as of ",raw.data$metadata$download_time,")", sep= ""))
}
# print(g1)
return(g1)
}


