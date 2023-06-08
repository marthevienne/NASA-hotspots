# rm(list=ls())
# 
# raw_data = readRDS("tracks_for_bssm")
# bssm = readRDS("predictedTracks_ssm")
# threshold = 10
# id = 'ct78d-D704-11'
# id = 'ct111-030-13'


my_SSM_filter <- function(raw_data, bssm, threshold) {
  new_bssm = matrix(nrow = 0, ncol = ncol(bssm))
  new_bssm = as.data.frame(new_bssm)
  colnames(new_bssm) = colnames(bssm)
  perc_of_RD = NULL
  mean_x.se_array = NULL
  mean_y.se_array = NULL
  sd_x.se_array = NULL
  sd_y.se_array = NULL
  
  for (id in unique(bssm$id)) {
    print(id)
    bssm_ind = bssm[bssm$id == id,]
    raw_data_ind = raw_data[raw_data$id == id,]
    targeted_bssm_row = which(bssm_ind$x.se >= threshold |
                                bssm_ind$y.se >= threshold)
    if (length(targeted_bssm_row) > 1) {
      bssm_rm = bssm_ind[targeted_bssm_row, ]
      raw_data_keep = NULL
      for (i in 1:nrow(bssm_rm)) {
        criteria = which(abs(raw_data_ind$date - bssm_rm$date[i]) == min(abs(raw_data_ind$date - bssm_rm$date[i])))
        if (length(criteria) > 1) {
          print(raw_data_ind[criteria, ])
          print(paste("Diff lat:", abs(diff(
            raw_data_ind$lat[criteria]
          ))))
          print(paste("Diff lon:", abs(diff(
            raw_data_ind$lon[criteria]
          ))))
        }
        if (length(criteria) > 0) {
          raw_data_keep = c(raw_data_keep, criteria[1])
        }
      }
      #length(unique(raw_data_keep))
      #print(length(raw_data_keep))
      raw_data_df = cbind("row_bssm" = raw_data_keep, raw_data_ind[raw_data_keep, c("date", "lon", "lat")])
    }
      new_bssm_ind = bssm_ind
      new_bssm_ind$raw_data = 0
      new_bssm_ind[targeted_bssm_row,] = NA
      new_bssm_ind$id[targeted_bssm_row] = id
      new_bssm_ind$lon[targeted_bssm_row] = raw_data_df$lon
      new_bssm_ind$lat[targeted_bssm_row] = raw_data_df$lat
      new_bssm_ind$date[targeted_bssm_row] = raw_data_df$date
      new_bssm_ind$raw_data[targeted_bssm_row] = 1
      new_bssm_ind = new_bssm_ind[!duplicated(new_bssm_ind), ]
      new_bssm = rbind(new_bssm, new_bssm_ind)
      perc_of_RD = c(perc_of_RD,
                     length(new_bssm_ind$raw_data[which(new_bssm_ind$raw_data == 1)]) / nrow(new_bssm_ind) * 100)
      mean_x.se_array = c(mean_x.se_array, mean(bssm_ind$x.se, na.rm = TRUE))
      mean_y.se_array = c(mean_y.se_array, mean(bssm_ind$y.se, na.rm = TRUE))
      sd_x.se_array = c(sd_x.se_array, sd(bssm_ind$x.se, na.rm = TRUE))
      sd_y.se_array = c(sd_y.se_array, sd(bssm_ind$y.se, na.rm = TRUE))
  }
  summary = as_tibble(cbind("id" = unique(bssm$id), "perc_of_raw_data" = perc_of_RD, 
                            "mean_x.SE" = mean_x.se_array, "sd_x.SE" = sd_x.se_array,
                            "mean_y.SE" = mean_y.se_array, "sd_y.SE" = sd_y.se_array))
  summary$perc_of_raw_data = as.numeric(summary$perc_of_raw_data)
  summary$mean_x.SE = as.numeric(summary$mean_x.SE)
  summary$mean_y.SE = as.numeric(summary$mean_y.SE)
  summary$sd_x.SE = as.numeric(summary$sd_x.SE)
  summary$sd_y.SE = as.numeric(summary$sd_y.SE)
  res = list("bssm" = new_bssm, "summary" = summary, "threshold" = threshold)
  return(res)
}

# plot(bssm_ind$lon, bssm_ind$lat)
# points(new_bssm$lon, new_bssm$lat, col = "red")
# bssm <- readRDS("tracks_for_bssm")
# bssm$date <- as.POSIXct(bssm$date, origin = "1970-01-01", tz = "GMT")
# str(bssm)
# 
# ## Path to save output figures
# path_fig <- "~/Dropbox/data/outputs_Marthe_2023/ssm_SES/"
# 
# ## Fit the SSM individual by individual
# i = 1
# 
# 
# ## Individual i
# this.id <- unique(bssm$id)[i]
# this.id = 'ct78d-D704-11'
# print(this.id)
# print(paste0("id: ", i, "/", length(unique(bssm$id))))
# 
# ## Subset by individual
# d <- bssm[bssm$id == this.id,]
# 
# ## Check track duration
# dur <- difftime(max(d$date), min(d$date), units = "hours")
# 
# ## Fit the SSM
# fit <-
#   fit_ssm(
#     d,
#     time.step = 4,
#     model = "crw",
#     vmax = 4,
#     control = ssm_control(verbose = 0)
#   )
# 
# plot(fit, what = "predicted")
# plot(fit, "p", type = 2, alpha = 0.1)
# 
# my.plot.ssm_df(fit, what = "predicted")
# 
# require(patchwork)
# # calculate & plot residuals
# t1 = Sys.time()
# res.crw <- osar(fit)
# t2 = Sys.time()
# 
# t2-t1
# 
# pdf("test_plot_res_SSM.pdf", height = 10, width = 12)
# (plot(res.crw, type = "ts") | plot(res.crw, type = "qq")) / 
#   (plot(res.crw, type = "acf") | plot_spacer())
# dev.off()
# 
# plot(fit$ssm$`ct78d-D704-11`$predicted$v, fit$ssm$`ct78d-D704-11`$predicted$v)
# 
# this.fit <- grab(fit, what = "predicted", as_sf = FALSE)
# 
# plot(this.fit$lon + fit$ssm$`ct78d-D704-11`$predicted$u, this.fit$lat + fit$ssm$`ct78d-D704-11`$predicted$v)
# 
# plot(d$lon, d$lat)
# 
# 
# hist(this.fit$x.se)
# plot(this.fit$x + this.fit$x.se, this.fit$y + this.fit$y.se)
# toto = this.fit[which(this.fit$x.se < 10 & this.fit$y.se < 10), ]
# plot(toto$x + toto$x.se, toto$y + toto$y.se)
# 
# plot(toto$x.se)
# 
# bssm_rm = this.fit[which(this.fit$x.se >= 10 & this.fit$y.se >= 10),]
# 
# diag_keep = NULL
# for (i in 1:nrow(bssm_rm)) {
#   diag_keep = c(diag_keep, which(abs(d$date - bssm_rm$date[i]) == min(abs(d$date - bssm_rm$date[i]))))
# }
# diag_keep = unique(diag_keep)
# 
# fmp <-
#   fit_mpm(fit,
#           what = "fitted",
#           model = "mpm",
#           control = mpm_control(verbose = 0))
