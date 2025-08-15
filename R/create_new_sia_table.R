# Part II Build out SIA data Frame
# Key Idea -> Use windows to identify SIAs build out subcomponets aligning with response to first SIA,
# if there is any virus detected at the country after
# Using Breakthrough window as 28 days from ending date of second SIA and following breakthrough
source(here::here("R/create_obx_dataset.R"))
source(here::here("R/gen_obx_br_rds.R"))

raw.data <- sirfunctions::get_all_polio_data(size="medium")


raw.data$pos <- raw.data$pos |>
                  dplyr::filter(!epid == "ISR-1-11-24-26")
# Nearest neighbor data
nn_first <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_raw/nn_fv_obs.rds")

obx_table <- gen_obx_table(raw.data,nn_first)

# # Testing
# master <- obx_table
#
# obx_table <- master |> dplyr::filter(ob_id == "YEM-cVDPV2-1")



# Global Inputs that don't change
# Global level inputs for table creations
v_type <- c("cVDPV 2", "cVDPV 3", "cVDPV 1")
v_type1 <- c("cVDPV 2", "cVDPV 3", "cVDPV 1",
             "cVDPV1andcVDPV2", "cVDPV2andcVDPV3","VDPV1andcVDPV2", "CombinationWild1-cVDPV 2")
start.date <- lubridate::as_date("2016-01-01")
end.date <-   lubridate::floor_date(lubridate::today())
cov_pct_lvl <- 6

# Global level data
# Local data load
positives.clean.01 <- raw.data[["pos"]] |>
  dplyr::filter(dateonset >=start.date & dateonset <=end.date) |>
  dplyr::filter(measurement %in% v_type) |>
  dplyr::arrange(place.admin.0, measurement, dateonset) |>
  dplyr::select(place.admin.0, place.admin.1, place.admin.2, adm0guid, adm1guid, admin2guid, epid, measurement, dateonset, emergencegroup,
                is.breakthrough, source, report_date, admin0whocode) |>
  dplyr::rename(adm2guid = admin2guid) |>
  dplyr::distinct(epid, measurement, .keep_all = T) |>
  dplyr::mutate(surv = dplyr::case_when(
    source == "AFP" ~ "AFP",
    source == "ENV" ~ "ES",
    source %in% c("Community", "Contact", "Healthy") ~ "Other")) |>
  #Manual fix for the epid causing problems in Yemen for T2
  dplyr::mutate(dateonset =
                  dplyr::if_else(epid == "YEM-ABY-2021-627-08-C6", lubridate::as_date("2021-09-29"),dateonset))



# Gen list to catch all round sub tables to merge after
obx_sia_rds <- list()
obx_sia_full_list <- list()

for (i in 1:nrow(obx_table)){
# x <- "ANG-cVDPV2-1"
x <- obx_table$ob_id[i]
df_sub <- obx_table |> dplyr::filter(ob_id == x)

# df_sub <- obx_table |> dplyr::filter(ob_id == "NIE-cVDPV2-2")


# Local Inputs that will change
ctry <- df_sub$ob_country
sero <- df_sub$ob_type
ob_start <- df_sub$ob_srt_d0
ob_end <- df_sub$most_recent
date_end <- df_sub$sia_date_upper

# Required Pop
dist <- raw.data$global.dist |>
  dplyr::filter(ADM0_NAME == ctry &
                  ENDDATE == "9999-12-31")

dist_c <- nrow(dist)

prov <- raw.data$global.prov |>
  dplyr::filter(ADM0_NAME == ctry &
                  ENDDATE == "9999-12-31")

prov_c <- nrow(prov)

# Create clean SIA dataset
sia_data  <- raw.data$sia |>
  dplyr::filter(yr.sia >= lubridate::year(start.date) &
                  status == "Done") |>
  dplyr::mutate(activity.end.date = dplyr::if_else(is.na(activity.end.date)==T,
                                                   activity.start.date,
                                                   activity.end.date),
                activity.end.date = dplyr::if_else(sia.code == "ETH-2021-002", lubridate::as_date("2021-11-15"),activity.end.date))

if(sero == "cVDPV 2"){

  sia_sub <- sia_data |>
    dplyr::filter(
      place.admin.0 == ctry &
        activity.start.date >= ob_start &
        activity.start.date <= date_end &
        vaccine.type %in% c("mOPV2", "tOPV", "nOPV2"))



}else if (sero == "cVDPV 1"){
  sia_sub <- sia_data |>
    dplyr::filter(place.admin.0 == ctry &
                    activity.start.date >= ob_start &
                    activity.start.date <= date_end &
                    vaccine.type %in% c("bOPV", "tOPV", "IPV + bOPV", "mOPV1"))



}else if(sero == "cVDPV 3"){
  sia_sub <- sia_data |>
    dplyr::filter(place.admin.0 == ctry &
                    activity.start.date >= ob_start &
                    activity.start.date <= date_end &
                    vaccine.type %in% c("bOPV", "tOPV", "IPV + bOPV", "mOPV3"))

}

sia_sub_all <- sia_sub |>
  # Filter out rounds labeled mop-up
  dplyr::mutate(activity.start.date = lubridate::as_date(activity.start.date),
                activity.end.date = lubridate::as_date(activity.end.date)) |>
  dplyr::arrange(activity.start.date, sia.code) |>
  dplyr::group_by(activity.start.date) |>
  dplyr::summarise(
    adm2guid = dplyr::first(adm2guid),
    sia_code = dplyr::first(sia.code), # keep only first sia.code to match
    code_count = dplyr::n_distinct(sia.code),
    sia_country = dplyr::first(place.admin.0),
    sia_dist = dplyr::n_distinct(place.admin.2),
    sia_prov = dplyr::n_distinct(place.admin.1),
    sia_type = dplyr::first(activity.type),
    sia_date = dplyr::first(activity.start.date),
    sia_date_end = dplyr::first(activity.end.date),
    sia_vac = dplyr::first(vaccine.type)) |>
  dplyr::select(-activity.start.date, -adm2guid) |>
  dplyr::arrange(sia_date) |>
  dplyr::mutate(
    ob_id = x,
    cov_pct_dist = sia_dist / dist_c * 100,
    cov_pct_prov = sia_prov / prov_c * 100) |>
  dplyr::arrange(sia_date) |>
  # Add in time difference in SIAS
  dplyr::mutate(
    sia_no = dplyr::row_number(),
    # Identify any R0s occuring within 14 days of the OB declaration
    ob_R0 = dplyr::if_else( sia_date > ob_start &
                     sia_date <= (ob_start + lubridate::days(14)), "Y", "N"),
    # Time between SIAS
    sia_time_diff = sia_date - dplyr::lag(sia_date, default= ob_start),
    # Time between SIAS
    mopup_check = dplyr::case_when(
      sia_type == "Mop-Up" ~ "Y",
      dplyr::row_number() != 1 & sia_type == "CR" & cov_pct_dist < cov_pct_lvl  & sia_time_diff <= 21 ~ "Y",
      TRUE ~ "N"),
    sia_cat = dplyr::case_when(
      mopup_check == "Y" | sia_type == "Mop-Up" ~ "3_mop-up",
      ob_R0 == "Y" ~ "1_R0",
      TRUE ~ "2_siard")) |>
  dplyr::arrange(sia_date, sia_cat)


# Identify first and second SIA responses


sia_sub2 <-  sia_sub_all |>
  dplyr::filter(sia_cat == "2_siard")

sia_int <- sia_sub2 |>
  dplyr::mutate(
    sia_rd_viz = dplyr::case_when(
      dplyr::row_number() == 1 ~ "first_sia",
      dplyr::row_number() == 2 ~ "second_sia",
      TRUE ~ NA_character_)) |>
  dplyr::select(sia_code, sia_rd_viz)

sia_sub_all <- dplyr::left_join(sia_sub_all, sia_int, by = "sia_code")



br_sub <- gen_obx_sia_br_rds(positives.clean.01,sia_sub2,df_sub)

obx_sia_full_list[[i]] <- sia_sub_all
obx_sia_rds[[i]] <- br_sub
cli::cli_alert(paste0(x, " outbreak completed"))
}

sia_full_all <- do.call(plyr::rbind.fill, obx_sia_full_list)

sia_obx_table <- do.call(plyr::rbind.fill, obx_sia_rds)

# Table aligns outbreaks current for second SIA Start / End date in first line
# TIme to breakthrough in second (first sia)
sia_obx_table <- sia_obx_table |>
                   dplyr::rename(
                    "time_to_int_str" = first_reg_sia,
                    "time_to_int_end" = first_reg_sia_end) |>
                   dplyr::select(-most_recent) |>
                   dplyr::relocate(ob_sia_id, .after = ob_id)


# Create a sensitivity analysis for identifying response and breakthrough rds
br_rd_codes <- sia_obx_table |>
                  dplyr::filter(is.na(sia_name)==F) |>
                  dplyr::pull(sia_name)


sia_full_all <- sia_full_all |>
                  dplyr::mutate(
                    sia_rd_viz =
                    dplyr::case_when(
                      is.na(sia_rd_viz) == F ~ sia_rd_viz,
                      is.na(sia_rd_viz) == T & sia_cat == "1_R0" ~ "round_0",
                      is.na(sia_rd_viz) == T & sia_cat == "3_mop-up" ~ "mopup_sia",
                      sia_code %in% br_rd_codes ~ "brk_sia",
                      TRUE ~ "extra_sias"))


# Need to investigate ETH-cVDPV2-1-5

rm(br_sub, df_sub, dist, obx_sia_rds, positives.clean.01, prov, sia_data, sia_sub, sia_sub2 , x)

