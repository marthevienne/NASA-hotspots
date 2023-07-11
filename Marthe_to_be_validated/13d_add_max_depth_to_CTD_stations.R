rm(list=ls())

## Add max depth to CTD stations table

stations <- readRDS("~/Desktop/WHOI/Data/output_data/ctd_stations_table_north_bound_interp")
profiles <- readRDS("~/Desktop/WHOI/Data/output_data/ctd_profiles_table")

stations <- stations %>%
  select(!contains("max_depth"))

id_stations <- stations %>%
  select(REF, station) %>%
  unique()

id_profiles <- profiles %>%
  select(REF, station) %>%
  unique()

toto <- profiles %>%
  filter(REF == "ct135-084BAT-14")
id_stations$REF[which(!(id_stations$REF %in% id_profiles$REF))]

max_depth_ctd <- profiles %>%
  group_by(REF, station) %>%
  reframe(max_depth = max(depth, na.rm = T))

max_depth_ctd %>%
  filter(is.na(max_depth))

stations <- stations %>%
  left_join(max_depth_ctd, by = c("REF", "station"))

stations %>%
  filter(is.na(max_depth))

saveRDS(stations, "~/Desktop/WHOI/Data/output_data/ctd_stations_table_north_bound_interp")
