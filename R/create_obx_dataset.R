
# Load data
raw.data <- sirfunctions::get_all_polio_data(size="medium")
# Nearest neighbor data
nn_first <- sirfunctions::edav_io(io = "read", default_dir = "GID/GIDMEA/giddatt", file_loc = "data_raw/nn_fv_obs.rds")
# IPV Coverage

# #Test Code
# obx_table <- gen_obx_table(raw.data,nn_first)

gen_obx_table <- function(raw.data, nn_first){
  #Load Internal Inputs
v_type <- c("cVDPV 2", "cVDPV 3", "cVDPV 1")
v_type1 <- c("cVDPV 2", "cVDPV 3", "cVDPV 1",
             "cVDPV1andcVDPV2", "cVDPV2andcVDPV3","VDPV1andcVDPV2", "CombinationWild1-cVDPV 2")

# IPV Only Using Countries to Flag
# Convert to spreadsheet or reference spatial object such as the map visulizations
ipv_only <- c("CANADA", "GERMANY", "FINLAND", "ISRAEL", "POLAND" , "SPAIN", "UKRAINE", "THE UNITED KINGDOM", "UNITED STATES OF AMERICA")
start.date <- lubridate::as_date("2016-01-01")
end.date <-   lubridate::floor_date(lubridate::today())
# Align data with download
active.end.date <- lubridate::floor_date(raw.data$metadata$download_time, "week", week_start = 7)
# Using one year
outbreak_window <- 365
# Using old SOP definition
breakthrough <- 28
# Threshold for percent of disticts included to identify small rounds for mop-ups, 20% of CRs are under 6% of districts
cov_pct_lvl <- 6


# set_parameters( breakthrough_min_date = 21,
#                 breakthrough_middle_date = 180,
#                 breakthrough_max_date = 365,
#                 detection_pre_sia_date = 90,
#                 start_date = start.date,
#                 end_date = end.date,
#                 recent_sia_start_year = lubridate::year(Sys.Date())-2)
#                 #This is used to restrict "Recent SIA with breakthrough transmission" figures to 'recent' SIAs in f.geompoint.case())

# AFP - 3543 / Post-AFP 3561 (8)
# ENV has 236 duplicates in Post
# Contact has 6 dup epids


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



# Identifies all outbreaks based on firsr virus that would trigger the OB response, add in First virus later as it may differ
# Extracts all outbreaks that are the first switches countries
df_ob <- positives.clean.01 |>
  dplyr::arrange(place.admin.0, measurement, report_date) |>
  dplyr::mutate(
    report_date  = lubridate::as_date(report_date),
    # Calculates time interval based on procceding virus in long_list format
    ob_flag = ifelse(place.admin.0 == dplyr::lag(place.admin.0, default = dplyr::first(place.admin.0)) &
                       measurement == dplyr::lag(measurement, default = dplyr::first(measurement)),
                     "TRUE", "FALSE"),
    ob_diff = report_date - dplyr::lag(report_date, default = dplyr::first(report_date))) |>
  dplyr::filter((dplyr::row_number()==1) |
           ob_flag == "FALSE") |>
  dplyr::filter(is.na(emergencegroup)==F)



# All other outbreaks of the same country that includes a year gap in the time between virus detections
# To strengthen can do a sensitivity check to see if the next virus included the NN data and was not realted to location ciruclation

df_sec <- positives.clean.01 |>
  dplyr::arrange(place.admin.0, measurement, dateonset) |>
  dplyr::mutate(
    ob_flag = ifelse(place.admin.0 == dplyr::lag(place.admin.0, default = dplyr::first(place.admin.0)) &
                       measurement == dplyr::lag(measurement, default = dplyr::first(measurement)),
                     "TRUE", "FALSE"),
    ob_diff = dateonset - dplyr::lag(dateonset, default = dplyr::first(dateonset))) |>
  dplyr::filter((
    (ob_flag == "TRUE" & ob_diff > outbreak_window))) |>
  dplyr:: filter(is.na(emergencegroup)==F)

#Pull Together
df_all <- dplyr::bind_rows(df_ob, df_sec)


# Rename variables - see guide
df_all <- df_all |>
  dplyr::rename(
    ob_country = place.admin.0,
    ob_srt_admin1 = place.admin.1,
    ob_srt_admin2 = place.admin.2,
    ob_srt_epid = epid,
    ob_srt_onset = dateonset,
    ob_type = measurement,
    ob_srt_eg = emergencegroup,
    ob_srt_source = source,
    ob_srt_d0 = report_date) |>
  dplyr::group_by(ob_country, ob_type) |>
  dplyr::mutate(ob_count = dplyr::row_number(),
         ob_type2 = stringr::str_replace_all(ob_type, " ", ""),
  #Create master ID for the Outbreak
         ob_id = paste0(admin0whocode, "-", ob_type2, "-", ob_count, sep = "")) |>
  dplyr::select(ob_id, ob_country, ob_srt_admin1,ob_srt_admin2, ob_srt_epid, ob_srt_onset,
         ob_type, ob_srt_eg,ob_srt_source, ob_srt_d0) |>
  dplyr::group_by(ob_country, ob_type) |>
  dplyr::mutate(
    ob_count = dplyr::row_number(),
    ob_overall = dplyr::n(),
    # Identify latest outbreak in countries when n ?> 1
    ob_status = ifelse(ob_count == ob_overall, "recent_ob", "prev_ob"))

# Add in first virus of the outbreak
df_first <- positives.clean.01 |>
  dplyr::arrange(place.admin.0, measurement, dateonset) |>
  dplyr::mutate(
    ob_flag = ifelse(place.admin.0 == dplyr::lag(place.admin.0, default = dplyr::first(place.admin.0)) &
                       measurement == dplyr::lag(measurement, default = dplyr::first(measurement)),
                     "TRUE", "FALSE"),
    diff = dateonset - dplyr::lag(dateonset, default = dplyr::first(dateonset))) |>
  dplyr::filter((dplyr::row_number()==1) |
           ob_flag == "FALSE" |
           (ob_flag == "TRUE" & diff > outbreak_window)) |>
  dplyr::filter(is.na(emergencegroup)==F) |>
  dplyr::group_by(place.admin.0, measurement) |>
  dplyr::mutate(ob_count = dplyr::row_number(),
         ob_type2 = stringr::str_replace_all(measurement, " ", ""),
         ob_id = paste0(admin0whocode, "-", ob_type2, "-", ob_count, sep = "")) |>
  dplyr::ungroup() |>
  dplyr::select(ob_id,
         epid,
         place.admin.1,
         place.admin.2,
         dateonset,
         source,
         emergencegroup,
         report_date) |>
  dplyr::rename(
    fv_epid = epid,
    fv_onset = dateonset,
    fv_eg = emergencegroup,
    fv_admin1 = place.admin.1,
    fv_admin2 = place.admin.2,
    fv_source = source,
    fv_rdate = report_date) |>
  dplyr::mutate(
    fv_rdate = lubridate::as_date(fv_rdate))


df_all <- dplyr::left_join(df_all, df_first, by = "ob_id")


# Identifies upper limits of each of the outbreaks time windows, needed for SIA data
# Removes the cVDPV1 LOA outbreak as it started in 2015

last_ob <- df_all |>
  dplyr::filter(ob_count == ob_overall) |>
  dplyr::select(ob_id, ob_country, ob_type)

t1a <- positives.clean.01 |>
  dplyr::arrange(place.admin.0, measurement, dateonset) |>
  dplyr::group_by(place.admin.0, measurement) |>
  dplyr::summarise(date_upper = dplyr::last(dateonset)) |>
  dplyr::filter(!(place.admin.0 == "LAO PEOPLE'S DEMOCRATIC REPUBLIC" & measurement == "cVDPV 1")) |>
  dplyr::mutate(sia_date_upper = lubridate::today())

last_ob <- dplyr::left_join(last_ob, t1a, by = c("ob_country"="place.admin.0", "ob_type"="measurement")) |>
  dplyr::ungroup() |>
  dplyr::select(ob_id, date_upper, sia_date_upper)

# Fill in the Upper limit of the date range for the outbreak to filter data for SIAs later one

df_all <- dplyr::left_join(df_all, last_ob, by = "ob_id") |>
  dplyr::arrange(ob_id) |>
  dplyr::mutate(date_upper = dplyr::if_else(is.na(date_upper)==T, dplyr::lead(fv_onset), date_upper),
         sia_date_upper = dplyr::if_else(is.na(sia_date_upper)==T, dplyr::lead(fv_onset), sia_date_upper))


#Remove secondary data frames - clean environment
rm(df_first, df_ob, df_sec, last_ob,t1a)



# Add in nearest neighbor data
# Get epids for polis download - may need a better way to do this
epids_first <- paste(df_all$fv_epid, sep="' '", collapse=", ")

# Load in match for first nearest neighbor -
nn_first <- nn_first |>
  dplyr::select(EPID, `Virus Type(s)`,  `Virus Date`, VdpvNtChangesClosestMatch, EPIDClosestMatch) |>
# Need to remove as its a duplicate from the epid based on the additional detection to get 1:1 match
  dplyr::filter(!(EPID == "ENV-DJI-ART--DOU-25-003" & `Virus Type(s)` == "VDPV2")) |>
  dplyr::select(-`Virus Type(s)`, -`Virus Date`) |>
  dplyr::rename(fv_nn_epid = EPIDClosestMatch,
         fv_nn_nt = VdpvNtChangesClosestMatch) |>
  # duplicated MAA, missing anyway, took care of additional cVDPV2 detections in DJI samples abov
  dplyr::distinct(EPID, .keep_all = T)


# Merge First virus NN
df_all <- dplyr::left_join(df_all, nn_first, by = c("fv_epid"="EPID")) |>
          dplyr::mutate(fv_yr = lubridate::year(fv_onset))
#Cleaning
rm(nn_first)



# Loop through data for each outbreak using defined windows to get epi count of detections,
# emergences, cases, es and other detections associated with each outbreak
# plus over current system as we lose this data if the outbreak is of the same emergence in country

ob_epi <- list()
for (i in 1:nrow(df_all)){
  x <- df_all$ob_id[i]
  df_sub <- df_all |> dplyr::filter(ob_id == x)

  id <- df_sub$ob_id
  date_start <- df_sub$fv_onset
  date_end <- df_sub$date_upper
  ctry <- df_sub$ob_country
  sero <- df_sub$ob_type

  ob_check <- positives.clean.01 |>
    dplyr::filter(place.admin.0 == ctry &
                  measurement == sero &
                  dateonset >= date_start)


  if(nrow(ob_check)==1){

    ob_viruses <- positives.clean.01 |>
      dplyr::filter(place.admin.0 == ctry &
               measurement == sero &
               dateonset >= date_start)
  }else{
    if(df_sub$ob_count == df_sub$ob_overall){
      ob_viruses <-  positives.clean.01 |>
        dplyr::filter(place.admin.0 == ctry &
                 measurement == sero &
                 dateonset >= date_start &
                 dateonset <= date_end)
    }else{
      ob_viruses <-  positives.clean.01 |>
        dplyr::filter(place.admin.0 == ctry &
                 measurement == sero &
                 dateonset >= date_start &
                 dateonset < date_end)
    }
  }

  ob_data <- ob_viruses |>
    dplyr::arrange(dateonset) |>
    dplyr::summarise(tot_no_detects = dplyr::n(),
              tot_no_emerg = dplyr::n_distinct(emergencegroup),
              across(surv, list(cases = ~ sum(. == "AFP"),
                                es = ~ sum(. == "ES"),
                                other = ~ sum(. == "Other"))),
              most_recent = dplyr::last(dateonset)) |>
    dplyr::mutate(ob_id = id)

  ob_epi[[i]] <- ob_data
  rm(ob_check, ob_data, ob_viruses, df_sub)
}

op_epi2 = do.call(rbind, ob_epi)

# Merge with overall dataset, include event vs outbreak definition variable
# From SK - define events as with one case and one es right now as we don't have travel history
# Combine into binary varaible for analytical purpose

df_all <- dplyr::left_join(df_all, op_epi2, by = "ob_id") |>
  dplyr::mutate(ob_cat = dplyr::case_when(
    tot_no_detects == 1 & surv_es == 1 ~ "es_evt",
    tot_no_detects == 1 & surv_cases == 1 ~ "case_evt",
    tot_no_detects >= 2 ~ "obx",
    TRUE ~ "obx_check"),
    ob_bin = dplyr::if_else(ob_cat == "obx", "obx", "evt")) |>
  dplyr::select(- ob_count, -ob_overall)

rm(ob_epi, op_epi2)

### need to set up a flag here if an OBX_check happens
# Tidy up data set
df_all <- df_all |> dplyr::select(ob_cat, ob_status, fv_yr, everything())

df_all <- df_all |>
           dplyr::relocate(ob_bin, .after = ob_cat) |>
           dplyr::relocate(ob_type, .after = ob_status) |>
           dplyr::rename(
             tot_cases = surv_cases,
             tot_es = surv_es,
             tot_other = surv_other,
             ob_no = ob_status) |>
   # Flag to determine the current status of the outbreak based on SOPs
           dplyr::mutate(ob_status = dplyr::case_when(
               most_recent >= lubridate::floor_date(active.end.date - months(6), "week", 7) ~ "1_active_u6mos",
               most_recent < lubridate::floor_date(active.end.date - months(6), "week", 7) &
               most_recent >= lubridate::floor_date(active.end.date - months(12), "week", 7)~ "2_active_6-12mos",
               TRUE ~ "3_inactive"),
   # Binary variable collasping all active catergories
               ob_status_bin = dplyr::case_when(
                 ob_status == "3_inactive" ~ "inactive",
                 ob_status %in% c("1_active_u6mos", "2_active_6-12mos") ~ "active"),
   # Identifies if country is within six months and does not expect to have conducted outbreak response
               ob_new = dplyr::if_else(ob_srt_d0 >= lubridate::floor_date(active.end.date - months(1), "week", 7), "1_y", "2_n"),
   # Uses manual list to identify IPV Countries, need to automate
               ipv_ctry = dplyr::if_else(ob_country %in% ipv_only, "IPV Only", NA_character_)) |>
          dplyr::relocate(ob_status, .after = ob_no) |>
          dplyr::relocate(ob_status_bin, .after = ob_status) |>
          dplyr::relocate(ob_new, .after = ob_status_bin) |>
          dplyr::relocate(ipv_ctry, .after = ob_new)




# Sen check - should we use 12 months or 13 months
  # idea is to use 12 months but wait until 13 months have passed in order to ensure


### Add in SIA data now - for table only trying to identify response to SIAs, clean-up SIA data and number of rounds to determine if we had a breakthrough virus
sia_data  <- raw.data$sia |>
  dplyr::filter(yr.sia >= lubridate::year(start.date) &
                  status == "Done")
ob_sias <- list()
sia_rds1 <- list()


# Identify all associated SIAs that are true rounds - will be needed for ob_sia table after
for (i in 1:nrow(df_all)){

# x <- "SOM-cVDPV3-1"
x <- df_all$ob_id[i]
df_sub <- df_all |> dplyr::filter(ob_id == x)

ob_start <- df_sub$ob_srt_d0
first_virus <- df_sub$fv_onset
date_end <- df_sub$sia_date_upper
ctry <- df_sub$ob_country
sero <- df_sub$ob_type
mr_virus <- df_sub$most_recent

# Get outbreak length to get median year of outbreak - rounded up for population estimates
yr_start <- df_sub$fv_yr
yr_end <- lubridate::year(df_sub$most_recent)
yrs <- c(yr_start, yr_end)
yr_pop<- round(median(yrs))
# Population - get midpoint of year of outbreak


# Pull out number of Admin1 regions
dist <- raw.data$global.dist |>
           dplyr::filter(ADM0_NAME == ctry &
                         ENDDATE == "9999-12-31")

dist_c <- nrow(dist)

prov <- raw.data$global.prov |>
  dplyr::filter(ADM0_NAME == ctry &
                  ENDDATE == "9999-12-31")

prov_c <- nrow(prov)

# population data
pop <- raw.data$dist.pop |>
  dplyr::filter(ADM0_NAME == ctry &
                yr.end == "9999" &
                year == yr_pop)

tpop_u15 <- sum(pop$u15pop, na.rm = T)
pop <- pop |>
        dplyr::select(u15pop, adm2guid)


# Assumption that outbreak area was started in the intial round
# RR - 400,000 (2-4 million)/ Min 2 million children,


# Identifies all potential SIAs associated with outbreak using cut-off windows

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


# Need to reduce here the number of SIAs. Examples of the same SIA code having multiple activity types with the same master code. Some are Mop-ups, others SIA dates
# Saving grace no duplicated admin 2 levels on each activity date. So will spread out we can combine all here
# may need to add in a list here for what SIA codes are in the main one

sia_sub2 <-  dplyr::left_join(sia_sub, pop, by = "adm2guid")


sia_sub2 <- sia_sub2 |>
  # Filter out rounds labeled mop-up
  dplyr::filter(!activity.type == "Mop-Up") |>
  dplyr::mutate(activity.start.date = lubridate::as_date(activity.start.date),
                activity.end.date = lubridate::as_date(activity.end.date),
  # Calculate window of SIA response
                sia_length = activity.end.date - activity.start.date) |>
  dplyr::arrange(activity.start.date, sia.code) |>
  dplyr::group_by(activity.start.date) |>
  dplyr::summarise(
    adm2guid = dplyr::first(adm2guid),
    sia_code = dplyr::first(sia.code), # keep only first sia.code to match
    code_count = dplyr::n_distinct(sia.code),
    sia_dist = dplyr::n_distinct(place.admin.2),
    sia_prov = dplyr::n_distinct(place.admin.1),
    sia_type = dplyr::first(activity.type),
    tot_kids = sum(u15pop, na.rm = T),
    sia_date = dplyr::first(activity.start.date),
    sia_date_end = dplyr::first(activity.end.date),
    sia_length = median(sia_length, na.rm =T),
    sia_vac = dplyr::first(vaccine.type)) |>
  dplyr::select(-activity.start.date, -adm2guid)


# Quality check to pull out mop-ups or R0 that are not codded as such
# Pull in last date calculation - # Same approach as R0 as going into break through - wanted to remove small rounds such as R0 or Mop-Ups; bigger ones to stay in

sia_rds <- sia_sub2 |>
  dplyr::arrange(sia_date) |>
  dplyr::mutate(
    ob_id = x,
    cov_pct_dist = sia_dist / dist_c * 100,
    cov_pct_prov = sia_prov / prov_c * 100,
    pct_kids_tar = round(tot_kids /tpop_u15 * 100)) |>
  dplyr::arrange(sia_date) |>
  # Add in time difference in SIAS
  dplyr::mutate(
    sia_no = dplyr::row_number(),
    # Identify any R0s occuring within 14 days of the OB declaration
    ob_R0 = dplyr::if_else(sia_date <= (ob_start + lubridate::days(14)), "Y", "N"),
    # Time between SIAS
    sia_time_diff = sia_date - dplyr::lag(sia_date, default= ob_start),
    # Identifies mop_up rounds from CR that are within 21 days of the last round, less than 6% of the districts covered and not a first SIA
    mopup_check = dplyr::case_when(
      sia_type == "Mop-Up" ~ "Y",
      dplyr::row_number() != 1 & sia_type == "CR" & cov_pct_dist < cov_pct_lvl  & sia_time_diff <= 21 ~ "Y",
      TRUE ~ "N"),
    sia_cat = dplyr::case_when(
      ob_R0 == "Y" ~ "1_R0",
      mopup_check == "Y" ~ "3_mop-up",
      TRUE ~ "2_siard")) |>
  dplyr::arrange(sia_date, sia_cat)


# # Pull out total number of SIA coded rounds:
sia_out <- sia_rds |>
  dplyr::summarise(
    id = x,
    tot_rds_dataset = dplyr::n(),
    sia_first = dplyr::first(sia_date),
    sia_last = dplyr::last(sia_date))



# # Focus on SNID / NIDS and CRS that do not appear as
# Pull out any that do not cover
# Some R0 can be big agains the
sia_out2 <-  sia_rds |>
  dplyr::filter(sia_cat =="2_siard" ) |>
         dplyr::summarise(
           id = x,
           snids_rds = dplyr::n(),
           first_reg_sia = dplyr::first(sia_date),
           sec_reg_sia = dplyr::nth(sia_date, 2),
           sec_reg_sia_end = dplyr::nth(sia_date_end, 2))
           # thrd_reg_sia = dplyr::nth(sia_date, 3),
           # last_reg_sia = dplyr::last(sia_date))

sia_out <- dplyr::left_join(sia_out, sia_out2, by = "id")


sia_rds1[[i]] <- sia_rds
ob_sias[[i]] <- sia_out
# rm(ob_check, ob_data, ob_viruses, df_sub)


}

ob_sias_all <- do.call(rbind, ob_sias)
sia_rds_all <- do.call(rbind, sia_rds1)


df_all2 <- dplyr::left_join(df_all, ob_sias_all, by = c("ob_id"="id")) |>
              dplyr::relocate(snids_rds, .after = tot_rds_dataset)

# rm(df_all, df_sub, dist, ob_sias, pop, prov, sia_out, sia_out2, sia_rds, sia_rds1, sia_sub, sia_sub2)

ob_table_final <- df_all2 |>
                    dplyr::mutate(
                      delayed_rd1 = dplyr::case_when(
                      snids_rds >= 1 & first_reg_sia >  (ob_srt_d0 + lubridate::dmonths(6)) ~ "1_y",
                      snids_rds >= 1 & first_reg_sia <= (ob_srt_d0  + lubridate::dmonths(6)) ~ "2_n",
                      snids_rds < 1 ~ "3_<1Rds"),
            # # Breakthrough Virus in Country - 28 days after second SIA completed from initial response
                    int_brk_vr = dplyr::case_when(
                             ob_new == "2_n" &  is.na(ipv_ctry) == T & snids_rds >= 2 & most_recent > (sec_reg_sia_end + lubridate::days(28)) ~ "1_y",
                             ob_new == "2_n" &  is.na(ipv_ctry) == T & snids_rds >= 2 & most_recent <= (sec_reg_sia_end + lubridate::days(28)) ~ "2_n",
                             ob_new == "2_n" &  is.na(ipv_ctry) == T & snids_rds <2 | ipv_ctry == "yes" ~ "3_<2Rds",
                             ob_new == "1_y" &  is.na(ipv_ctry) == T ~ "4_newob",
                             ipv_ctry == "IPV Only" ~ "5_ipvctry"),
            # Surveillance Delay - if Epid from virus that was first reported based on report_date differs first virus based on onset
             delayed_surv = dplyr::if_else(
                            fv_epid == ob_srt_epid, "2_n", "1_y"))

return(ob_table_final)
}




