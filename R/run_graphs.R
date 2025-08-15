# Master Graph Test

source(here::here("R/create_new_sia_table.R"))
source(here::here("R/run_obx_graphs.R"))

run_obx_sia_graph(
  raw.data = raw.data,
  obx_table = obx_table,
  sia_full_all = sia_full_all,
  sia_obx_table = sia_obx_table,
  run_type = "active_ob",
  save_graph = "yes",
  cdc_upload = "yes",
  viz = "sen_check"
)



