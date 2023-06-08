rm(list=ls())

library(ggplot2)
library(ggpubr)

setwd("~/Desktop/WHOI/Data/")

raw_data = readRDS("tracks_for_bssm") # ARGOS data prepared for BSSM
bssm = readRDS("predictedTracks_ssm") # predicted tracks after BSSM

length(raw_data[!duplicated(raw_data),])/length(raw_data$date)*100 # doit être égal à 0

## Sampled tracks
id_test = c('ct78d-D704-11', 'ct78d-D827-11', 'ct109-968-14', 'ct111-030-13', 'ct116-13-15', 'ct120-688BAT-15', "ct164-482-21")

## 
raw_data =  subset(raw_data, subset = id %in% id_test)
bssm = subset(bssm, subset = id %in% id_test)

seals_id = unique(raw_data$id)

thr_1 = 10
thr_2 = 20
thr_3 = 30
thr_4 = 40
thr_5 = 50
thr_6 = 60
thr_7 = 70
thr_8 = 80
thr_9 = 90
thr_10 = 100

filt_pred_SSM_1 = my_SSM_filter(raw_data, bssm, threshold = thr_1)
filt_pred_SSM_2 = my_SSM_filter(raw_data, bssm, threshold = thr_2)
filt_pred_SSM_3 = my_SSM_filter(raw_data, bssm, threshold = thr_3)
filt_pred_SSM_4 = my_SSM_filter(raw_data, bssm, threshold = thr_4)
filt_pred_SSM_5 = my_SSM_filter(raw_data, bssm, threshold = thr_5)
filt_pred_SSM_6 = my_SSM_filter(raw_data, bssm, threshold = thr_6)
filt_pred_SSM_7 = my_SSM_filter(raw_data, bssm, threshold = thr_7)
filt_pred_SSM_8 = my_SSM_filter(raw_data, bssm, threshold = thr_8)
filt_pred_SSM_9 = my_SSM_filter(raw_data, bssm, threshold = thr_9)
filt_pred_SSM_10 = my_SSM_filter(raw_data, bssm, threshold = thr_10)

filt_pred_SSM = list(filt_pred_SSM_1, filt_pred_SSM_2, filt_pred_SSM_3, 
                     filt_pred_SSM_4, filt_pred_SSM_5, filt_pred_SSM_6,
                     filt_pred_SSM_7, filt_pred_SSM_8, filt_pred_SSM_9,
                     filt_pred_SSM_10)

for (i in id_test) {
  bssm_ind = subset(bssm, subset = id == i, select = c(date, lon, lat))
  bssm_ind$type = "pre-filtered SSM"
  raw_data_ind =  subset(raw_data, subset = id == i, select = c(date, lon, lat))
  raw_data_ind$type = "ARGOS"
  
  pdf(sprintf("id_%s_my_SSM_plot.pdf", i), height = 10, width = 16)
  for (pred_SSM in filt_pred_SSM) {
    
    filt_pred_SSM_ind =  subset(pred_SSM$bssm, subset = id == i, select = c(date, lon, lat))
    filt_pred_SSM_ind$type = "filtered SSM"
    
    df = rbind(bssm_ind, raw_data_ind, filt_pred_SSM_ind)
    
    title = sprintf("%s | threshold = %.0f", i, pred_SSM$threshold)
    
    # pLat <- ggplot(raw_data_ind, aes(x = date, y = lat, shape = id)) +
    #   geom_point(size = .3, shape = 3, show.legend = T) +
    #   geom_point(data = bssm_ind, aes(x = date, y = lat, col = id), size = .5, show.legend = T)+
    #   geom_point(data = filt_pred_SSM_ind, aes(x = date, y = lat, col = as.factor(raw_data)), size = .3, show.legend = T) +
    #   ggtitle(title) +
    #   labs(colour = "Type of data", shape = "ARGOS data")+
    #   xlab("Date")+
    #   ylab("Lat (°N)")
    # 
    # pLon <- ggplot(raw_data_ind, aes(x = date, y = lon)) +
    #   geom_point(size = .3, shape = 3) +
    #   geom_point(data = bssm_ind, aes(x = date, y = lon), size = .5, col = "pink")+
    #   geom_point(data = filt_pred_SSM_ind, aes(x = date, y = lon, col = as.factor(raw_data)), size = .3, shape= 4) +
    #   ggtitle(title)+
    #   labs(colour = "Type of data")+
    #   xlab("Date")+
    #   ylab("Lon (°E)")
    
    pLat <- ggplot(df, aes(x = date, y = lat, shape = type, col = type), size = .1) +
      geom_point() +
      ggtitle(title) +
      labs(colour = "Type of data", shape = "Type of data")+
      xlab("Date")+
      ylab("Lat (°N)")+
      scale_shape_manual(values = c(3, 16, 4))+
      scale_color_viridis(discrete = TRUE)

    pLon <- ggplot(df, aes(x = date, y = lon, shape = type, col = type), size = .1) +
      geom_point() +
      ggtitle(title) +
      labs(colour = "Type of data", shape = "Type of data")+
      xlab("Date")+
      ylab("Lon (°E)")+
      scale_shape_manual(values = c(3, 16, 4))+
      scale_color_viridis(discrete = TRUE)
    
    p <- ggarrange(pLat, pLon, ncol = 2, nrow = 1)
  
    print(p)
  }
  dev.off()
}

#-----Save output
sink("my_SSM_filter_summary.txt")
cat(paste("Last modified :", Sys.Date()))
cat("\n")
sink()
#----------------

for (pred_SSM in filt_pred_SSM) {
  sink("my_SSM_filter_summary.txt", append = T)
  print(paste("Threshold :", pred_SSM$threshold))
  cat("\n")
  print(pred_SSM$summary)
  cat("\n")
  cat("\n")
  sink()
  #----------------
}


############
cf_lon = bssm$lon[1]/bssm$x[1]
cf_lat = bssm$lat[1]/bssm$y[1]

bxpLat <- ggplot() +
  geom_boxplot(data = bssm, aes(x = id, y = y.se*cf_lat)) +
  ylab("Standard error on latitude (°)") +
  xlab("Track") +
   theme_bw()

bxpLon <- ggplot() +
  geom_boxplot(data = bssm, aes(x = id, y = x.se*cf_lon)) +
  ylab("Standard error on longitude (°)") +
  xlab("Track") +
  theme_bw()

pdf("boxplots_SE_sampled_tracks.pdf", height = 6, width = 12)
print(bxpLat)
print(bxpLon)
dev.off()

bssm_sub = subset(bssm, subset = lat < -58.5)
summary(bssm_sub$y.se*cf_lat)
summary(bssm_sub$x.se*cf_lon)

bxpLat <- ggplot() +
  geom_boxplot(data = bssm_sub, aes(x = id, y = y.se*cf_lat)) +
  ylab("Standard error on latitude (°)") +
  xlab("Track") +
  theme_bw()

bxpLon <- ggplot() +
  geom_boxplot(data = bssm_sub, aes(x = id, y = x.se*cf_lon)) +
  ylab("Standard error on longitude (°)") +
  xlab("Track") +
  theme_bw()

pdf("boxplots_SE_NB_sampled_tracks.pdf", height = 6, width = 12)
print(bxpLat)
print(bxpLon)
dev.off()

###############
id = "ct111-030-13"
threshold = 100

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
    targeted_bssm_row = which(bssm_ind$x.se >= threshold &
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
    if (length(targeted_bssm_row) > 1) {
      new_bssm_ind[targeted_bssm_row,] = NA
      new_bssm_ind$id[targeted_bssm_row] = id
      new_bssm_ind$lon[targeted_bssm_row] = raw_data_df$lon
      new_bssm_ind$lat[targeted_bssm_row] = raw_data_df$lat
      new_bssm_ind$date[targeted_bssm_row] = raw_data_df$date
      new_bssm_ind$raw_data[targeted_bssm_row] = 1
      new_bssm_ind = new_bssm_ind[!duplicated(new_bssm_ind), ]
      perc_of_RD = c(perc_of_RD,
                     length(new_bssm_ind$raw_data[which(new_bssm_ind$raw_data == 1)]) / nrow(new_bssm_ind) * 100)
    } else {
      perc_of_RD = c(perc_of_RD, 0)
    }
    new_bssm = rbind(new_bssm, new_bssm_ind)
    mean_x.se_array = c(mean_x.se_array, mean(bssm_ind$x.se, na.rm = TRUE))
    mean_y.se_array = c(mean_y.se_array, mean(bssm_ind$y.se, na.rm = TRUE))
    sd_x.se_array = c(sd_x.se_array, sd(bssm_ind$x.se, na.rm = TRUE))
    sd_y.se_array = c(sd_y.se_array, sd(bssm_ind$y.se, na.rm = TRUE))
  }
  summary = as.data.frame(cbind("id" = unique(bssm$id), "perc_of_raw_data" = perc_of_RD, 
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
