end <- df_sub$date_upper

test <-  raw.data[["pos"]] |>
  dplyr::filter(dateonset >=ob_start & dateonset <= end & place.admin.0 == ctry & measurement == sero) |>
  dplyr::filter(measurement %in% v_type) |>
  dplyr::arrange(place.admin.0, measurement, dateonset) |>
  dplyr::select(place.admin.0, place.admin.1, place.admin.2, epid, measurement, dateonset, emergencegroup,
               report_date)


test <- sia_data |>
            dplyr::group_by(sia.code) |>
            dplyr::count(activity.type) |>
            tidyr::pivot_wider(names_from = activity.type, values_from = n)



test <- sia_sub |>
  dplyr::group_by(activity.start.date, place.admin.2) |>
  dplyr::count()


# Test if SIA rounds make an impact
# Too much variablity to use
test_sia <- sia_data |>
                dplyr::select(sia.code, activity.start.date, activity.end.date, activity.type) |>
                dplyr::mutate(
                  activity.start.date = lubridate::as_date(activity.start.date),
                  activity.end.date = lubridate::as_date(activity.end.date),
                  sia_time = activity.end.date - activity.start.date)

table(test_sia$activity.type, test_sia$sia_time)

tapply(sia_rds_all$cov_pct_dist, sia_rds_all$sia_type, summary)

# Won't work after
test_12 <- df_all
test_13 <- df_all2

test1 <- dplyr::anti_join(df_all, df_all2, by = "ob_id")

#Events
pv_envts <- df_all |>  dplyr::select(ob_id, ob_status, ob_country, ob_srt_d0, date_upper,most_recent, sia_date_upper)

tapply(sia_rds_all$sia_length, sia_rds_all$sia_type, summary)
