# Part II Build out SIA data Frame
# Key Idea -> Use windows to identify SIAs build out subcomponets aligning with response to first SIA,
# if there is any virus detected at the country after
# Using Breakthrough window as 28 days from ending date of second SIA and following breakthrough
source(here::here("R/create_obx_dataset.R"))

# Load data
raw.data <- sirfunctions::get_all_polio_data(size="medium")
# Nearest neighbor data
nn_first <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_raw/nn_fv_obs.rds")

obx_table <- gen_obx_table(raw.data,nn_first)



x <- "ANG-cVDPV2-2"
# x <- df_all$ob_id[i]
df_sub <- obx_table |> dplyr::filter(ob_id == x)

# Global level inputs for table creations
v_type <- c("cVDPV 2", "cVDPV 3", "cVDPV 1")
v_type1 <- c("cVDPV 2", "cVDPV 3", "cVDPV 1",
             "cVDPV1andcVDPV2", "cVDPV2andcVDPV3","VDPV1andcVDPV2", "CombinationWild1-cVDPV 2")
start.date <- lubridate::as_date("2016-01-01")
end.date <-   lubridate::floor_date(lubridate::today())
date_end <- df_sub$sia_date_upper
cov_pct_lvl <- 6

ob_start <- df_sub$ob_srt_d0
ob_end <- df_sub$most_recent
sia_end <- df_sub$sia_date_upper
ctry <- df_sub$ob_country
sero <- df_sub$ob_type
sia2 <- df_sub$sec_reg_sia_end
b_virus <- df_sub$int_brk_vr


# Load in Clean Global Data for viruses:

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
    source %in% c("Community", "Contact", "Healthy") ~ "Other"))

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
                  status == "Done")

if(sero == "cVDPV 2"){

  sia_sub <- sia_data |>
    dplyr::filter(
      place.admin.0 == ctry &
        activity.start.date >= ob_start &
        activity.start.date <= date_end &
        vaccine.type %in% c("mOPV2", "tOPV", "nOPV2"))



}else if (sero %in% c("cVDPV 1")){
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




sia_sub2 <- sia_sub |>
  # Filter out rounds labeled mop-up
  dplyr::filter(!activity.type == "Mop-Up") |>
  dplyr::mutate(activity.start.date = lubridate::as_date(activity.start.date),
                activity.end.date = lubridate::as_date(activity.end.date)) |>
  dplyr::arrange(activity.start.date, sia.code) |>
  dplyr::group_by(activity.start.date) |>
  dplyr::summarise(
    adm2guid = dplyr::first(adm2guid),
    sia_code = dplyr::first(sia.code), # keep only first sia.code to match
    code_count = dplyr::n_distinct(sia.code),
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
    ob_R0 = dplyr::if_else(sia_date <= (ob_start + lubridate::days(14)), "Y", "N"),
    # Time between SIAS
    sia_time_diff = sia_date - dplyr::lag(sia_date, default= ob_start),
    # Time between SIAS
    mopup_check = dplyr::case_when(
      sia_type == "Mop-Up" ~ "Y",
      dplyr::row_number() != 1 & sia_type == "CR" & cov_pct_dist < cov_pct_lvl  & sia_time_diff <= 21 ~ "Y",
      TRUE ~ "N"),
    sia_cat = dplyr::case_when(
      ob_R0 == "Y" ~ "1_R0",
      mopup_check == "Y" ~ "3_mop-up",
      TRUE ~ "2_siard")) |>
  dplyr::arrange(sia_date, sia_cat) |>
  dplyr::filter(sia_cat == "2_siard")


# Identify next sia after breakthrough
# For Monday - Filter sia_sub2 to the first SIA after the breakthrough virus onset or report date - report_date
# Merge into data set with line
# Get col names in order on data set with master line
# Find out how to loop through each

# x <- df_all$ob_id[i]

base <- df_sub |>
  dplyr::ungroup() |>
  dplyr::select(ob_id, ob_country, ipv_ctry, ob_srt_epid, ob_srt_onset, ob_srt_d0, sec_reg_sia, sec_reg_sia_end, most_recent, int_brk_vr)


#Identify  the first breakthrough virus was after the breakthrough
next_virus <- positives.clean.01 |>
  dplyr::filter(
    place.admin.0 == ctry &
      measurement == sero &
      dateonset > (sia2 + lubridate::days(28))) |>
  dplyr::arrange(report_date, dateonset) |>
  dplyr::select(epid, place.admin.0, place.admin.1, dateonset, report_date) |>
  dplyr::summarise(
    ob_id = x,
    ob_country = dplyr::first(place.admin.0),
    ob_srt_epid = dplyr::first(epid),
    ob_srt_onset = dplyr::first(dateonset),
    ob_srt_d0 = dplyr::first(report_date),
    most_recent = last(dateonset))

# Can also arrange on second col and merg variables here

sia_next <- next_virus$ob_srt_d0


# Bring in SIA Data
next_sia <- sia_sub2 |>
              dplyr::filter(sia_date >= sia_next) |>
              dplyr::summarise(
                sec_reg_sia = dplyr::first(sia_date),
                sec_reg_sia_end = dplyr::first(sia_date_end))

next_virus <- dplyr::bind_cols(next_virus, next_sia)

b1 <- base |>
        dplyr::select(ob_id, ipv_ctry)

next_virus <- dplyr::left_join(next_virus, b1, by = "ob_id") |>
  dplyr::relocate(ipv_ctry, .after = ob_country)
next_virus <-    next_virus |>
                 dplyr::mutate(int_brk_vr = dplyr::case_when(
                   is.na(ipv_ctry) == T  & most_recent > (sec_reg_sia_end + lubridate::days(28)) ~ "1_y",
                   is.na(ipv_ctry) == T  & most_recent <= (sec_reg_sia_end + lubridate::days(28)) ~ "2_n",
                   is.na(ipv_ctry) == F ~ "5_ipvctry"))|>
  dplyr::relocate(most_recent, .after = sec_reg_sia_end)

##### Next part will need an if/else here for multiple ones

br_sb <- dplyr::bind_rows(base, next_virus) |>
           dplyr::mutate(subid = dplyr::row_number(),
                         ob_sia_id = paste0(ob_id, "-", subid)) |>
          dplyr::select(-subid)


# Add in ID after, can bring in down after to make the id, combine with the first line after - add in the sublabels afterwards

