# Generate map that shows all detections with all SIAs detections to ensure that the detecitons and SIA windows line
# Will move over to SIR functions after

# Look into Chad's outbreak
# Because of extended SIAs - anchored on the first date - 3 plus 28 to get the breakthrough window



gen_obx_map_reg_sia<- function(df_sub, raw.data, sia_obx_table, x){

ob_start <- df_sub$fv_onset - lubridate::month(1)
sia1_start <- df_sub |> dplyr::pull(first_reg_sia)
sia1_end   <- df_sub |> dplyr::pull(first_reg_sia_end)
sia2_start <- df_sub |> dplyr::pull(sec_reg_sia)
sia2_end <- df_sub |> dplyr::pull(sec_reg_sia_end)
ob_sia_start <- df_sub$ob_srt_d0
ob_end <- df_sub$sia_date_upper
ctry <- df_sub$ob_country
sero <- df_sub$ob_type

ob_min <- (df_sub$fv_onset - lubridate::dmonths(1))
if(df_sub$ob_status == "1_active_u6mos"){
  ob_max <- lubridate::today()
}else{
ob_max  <- df_sub$most_recent + lubridate::dmonths(12)
}
if(sero == "cVDPV 1"){
  virus <- c("cVDPV 1", "VDPV 1")
}else if(sero == "cVDPV 2"){
  virus <- c("cVDPV 2", "VDPV 2")
}else if(sero == "cVDPV 3"){
  virus <- c("cVDPV 3", "VDPV 3")
}else{}

# SIA Data
sia_phases <- sia_obx_table |> dplyr::filter(ob_id == x)



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
day0_epids <- sia_obx_table |>
  dplyr::filter(ob_id == x) |>
  dplyr::pull(ob_srt_epid)

day0_data <- raw.data$pos |>
  dplyr::filter(epid %in% day0_epids) |>
  dplyr::mutate(measurement = "Day 0 Virus")

# Pull in SIA Data

# For now just done responses
region_virus <- unique(v_map$place.admin.1)
sia_1 <- unique(unlist(v_map$sias))


sia_data_sub <- raw.data$sia |>
  dplyr::filter( place.admin.0 %in% ctry &
             place.admin.1 %in% region_virus &
             sub.activity.end.date >= ob_sia_start &
             sub.activity.end.date <= ob_end &
             vaccine.type %in% sia_1 &
             status %in% c("Done")) |>
  dplyr::mutate(activity.start.date = lubridate::ymd(activity.start.date),
         activity.end.date = lubridate::ymd(activity.end.date),
         sia_proxy = "SIA",
         status = factor(status)) |>
  dplyr::arrange(place.admin.0, place.admin.1, activity.start.date) |>
  dplyr::distinct(sia.code, place.admin.1, .keep_all = TRUE)


g1 <- ggplot2::ggplot() +
      ggplot2::geom_point(data=v_map, ggplot2::aes(x=place.admin.1, y = dateonset, shape = measurement, color = emergencegroup),  size = 3) +
      ggplot2::geom_point(data=day0_data, ggplot2::aes(x=place.admin.1, y = dateonset, shape = measurement), size = 6, stroke = 1) +
      ggplot2::scale_color_manual(name = "Emergence Groups:",
                                  values = emg_cols) +
      ggplot2::coord_flip() +
      ggplot2::xlab("Impacted Region") +
      ggplot2::ylab("Date of Onset / Collection") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_date(date_breaks = "4 months",
                            date_labels = "%b '%y",
                            limits = c(lubridate::as_date(ob_min), lubridate::as_date(ob_max))) +
  ggplot2::geom_errorbar(data=sia_data_sub,
                ggplot2::aes(x=place.admin.1,
                    y = activity.start.date,
                    ymin=activity.start.date,
                    ymax = activity.start.date,
                    linetype = status),
                    width = 0.75,
                    show.legend = F) +
  ggplot2::scale_shape_manual(name = "Detection Type:",
                         values = c(
                           "VDPV 1" =  8,
                           "VDPV 2" =  8,
                           "VDPV 3" =  8,
                           "cVDPV 2" = 16,
                           "cVDPV 1" = 16,
                           "cVDPV 3" = 16,
                           "WILD 1" = 15,
                           "Day 0 Virus" = 1),
                         labels = c( "VDPV 1" =  "VDPV1",
                                     "VDPV 2" =  "VDPV2",
                                     "VDPV 3" =  "VDPV3",
                                     "cVDPV 2" = "cVDPV2",
                                     "cVDPV 1" = "cVDPV1",
                                     "cVDPV 3" = "cVDPV3",
                                     "WILD 1" = "WPV1",
                                     "Day 0 Virus" = "Day 0 Virus"),
                         breaks = c("Day 0 Virus", "cVDPV 1", "cVDPV 2", "cVDPV 3",
                                    "VDPV 1", "VDPV 2", "VDPV 3", "WILD 1"), drop = F) +
  ggplot2::geom_rect(data = sia_phases,
  ggplot2::aes(xmin = -Inf,
                xmax = Inf,
                ymin = lubridate::as_date(ob_srt_d0), ymax = lubridate::as_date(br_viz) + lubridate::days(3)), fill = "grey40", alpha = .2) +
  ggplot2::geom_rect(data = sia_phases,
                     ggplot2::aes(xmin = -Inf,
                                  xmax = Inf,
                                  ymin = (lubridate::as_date(br_viz) + lubridate::days(3)), ymax = (lubridate::as_date(br_viz) + lubridate::days(28))), fill = "blue", alpha = .2) +

  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 12, face = "bold"),
    axis.text =  ggplot2::element_text(size = 10),
    legend.position = "bottom",
    legend.key = ggplot2::element_blank(),
    legend.title.position = "top",
    legend.direction = "horizontal",
    # legend.spacing.x = unit(0.05, 'cm'),
    legend.text = ggplot2::element_text(size = 10),
    plot.title.position = "plot",
    plot.title = ggplot2::element_text(size = 12,
                              face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 10)) +
  ggplot2::guides(
    color = ggplot2::guide_legend(order = 2, nrow = 3),
    shape = ggplot2::guide_legend(order = 1, override.aes = list(size=3), ncol=1)) +
  ggplot2::labs(
    title = paste0(df_sub$ob_country, ": ", df_sub$ob_type, " Outbreak (", df_sub$ob_id, ")"),
    subtitle = paste0("Status: ", df_sub$ob_status_bin),
    caption = paste0("Solid lines repersent completed SIA Rounds. Grey Shading indicates time from Day 0 to 2nd or Breakthrough SIA.\n",
                     "Blue shading indicate 28 day washout period following 2nd or breakthrough SIA round.\n",
                     "Produced by: CDC-CGH-GID-PEB. Source: POLIS (Data as of ",raw.data$metadata$download_time,")", sep= ""))


return(g1)
}

