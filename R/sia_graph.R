# Generate map that shows all detections with all SIAs detections to ensure that the detecitons and SIA windows line
# Will move over to SIR functions after

x <- "COD-cVDPV1-1"
df_sub <- obx_table |> dplyr::filter(ob_id == x)

ob_start <- df_sub$fv_onset - lubridate::month(1)
sia1_start <- df_sub |> dplyr::pull(first_reg_sia)
sia1_end   <- df_sub |> dplyr::pull(first_reg_sia_end)
sia2_start <- df_sub |> dplyr::pull(sec_reg_sia)
sia2_end <- df_sub |> dplyr::pull(sec_reg_sia_end)
ob_end <- df_sub$sia_date_upper
ctry <- df_sub$ob_country
sero <- df_sub$ob_type

ob_min <- (df_sub$fv_onset - lubridate::dmonths(1))
ob_max  <- df_sub$sia_date_upper + lubridate::dmonths(1)

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
                                       TRUE ~ emergencegroup)) |>
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






g1 <- ggplot2::ggplot() +
      ggplot2::geom_point(data=v_map, ggplot2::aes(x=place.admin.1, y = dateonset, shape = measurement, color = emergencegroup), alpha = 0.6, size = 3) +
      ggplot2::geom_point(data=day0_data, ggplot2::aes(x=place.admin.1, y = dateonset, shape = measurement), size = 6, stroke = 1, alpha = 0.8) +
      ggplot2::scale_color_manual(name = "Emergence Colors:",
                                  values = emg_cols) +
      ggplot2::coord_flip() +
      ggplot2::xlab("Impacted Region") +
      ggplot2::ylab("Date of Onset / Collection") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_date(date_breaks = "4 months",
                            date_labels = "%b '%y",
                            limits = c(lubridate::as_date(ob_min), lubridate::as_date(ob_max))) +
    ggplot2::scale_shape_manual(name = "Virus:",
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
                                     "Day 0 Virus" = "Day 0 Virus")) +
  ggplot2::geom_rect(data = sia_phases,
  ggplot2::aes(xmin = -Inf,
                xmax = Inf,
                ymin = lubridate::as_date(ob_srt_d0), ymax = lubridate::as_date(br_viz)), fill = "grey40", alpha = .2) +
  ggplot2::geom_rect(data = sia_phases,
                     ggplot2::aes(xmin = -Inf,
                                  xmax = Inf,
                                  ymin = lubridate::as_date(br_viz), ymax = (lubridate::as_date(br_viz) + lubridate::days(28))), fill = "blue", alpha = .2) +
  # S
  ggplot2::geom_rect(data = sia_phases,
  ggplot2::aes(xmin = -Inf,
               xmax = Inf,
               ymin = lubridate::as_date(time_to_int_str), ymax = lubridate::as_date(time_to_int_end)), fill = "black" ) +

  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 12, face = "bold"),
    axis.text =  ggplot2::element_text(size = 10),
    legend.position = "bottom",
    legend.key = ggplot2::element_blank(),
    legend.direction = "horizontal",
    # legend.spacing.x = unit(0.05, 'cm'),
    legend.text = ggplot2::element_text(size = 10),
    plot.title.position = "plot",
    plot.title = ggplot2::element_text(size = 12,
                              face = "bold"),
    plot.subtitle = ggplot2::element_text(size = 8))

print(g1)

# Extra Code
      geom_errorbar(data=sia_data_sub,
                    aes(x=place.admin.1,
                        y = activity.start.date,
                        ymin=activity.start.date,
                        ymax = activity.start.date,
                        linetype = status),
                    width = 0.75) +

      scale_linetype_manual(name = "SIA Response:",
                            values = c("Done" = 1,
                                       "Planned" = 4)) +
      guides(
        color = guide_legend(order = 1),
        shape = guide_legend(order = 2, override.aes = list(size=3)),
        linetype = guide_legend(order = 3)) +

      labs(
        caption = paste0("AFP: Acute flaccid paralysis surveillance, cVDPV: Circulating vaccine-derived poliovirus, ES: Environmental surveillance, SIA: Supplementary immunization activity, WPV: Wild poliovirus \n",
                         "Other surveillance sources include: close & community contacts & healthy children sampling \n",
                         "Produced by: ",owner,". Source: POLIS (Data as of ",date_1,")", sep= ""))

