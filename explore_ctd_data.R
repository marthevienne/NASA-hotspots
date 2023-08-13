rm(list=ls())

ctd <- readRDS("~/Desktop/WHOI/Data/output_data/ctd_stations_table_north_bound_interp")

time_diff <- ctd %>%
  group_by(REF) %>%
  arrange(time) %>%
  # filter(REF == "ct134-277-14") %>%
  reframe(dt = diff(time))

summary(as.numeric(time_diff$dt) / 60 / 60)

summary_time_diff <- time_diff %>%
  group_by(REF) %>%
  reframe(mean_dt = mean(dt),
          median_dt = median(dt))

pdf("~/Dropbox/data/outputs_Marthe_2023/ctd/hist_mean_time_interval_stations.pdf",
  height = 5,
  width = 8
)
hist(as.numeric(summary_time_diff$mean_dt) / 60 / 60, breaks = 50,
    xlab = "Mean time interval (h)", 
    main = "Mean time between CTD stations")
dev.off()


pdf("~/Dropbox/data/outputs_Marthe_2023/ctd/hist_median_time_interval_stations.pdf",
    height = 5,
    width = 8
)
hist(as.numeric(summary_time_diff$median_dt) / 60 / 60, breaks = 50,
     xlab = "Median time interval (h)", 
     main = "Median time between CTD stations")
dev.off()
