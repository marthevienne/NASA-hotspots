rm(list=ls())

library(lubridate)
library(stringr)

source("~/Desktop/WHOI/Codes/palettes/palette_polynya.R")

setwd("~/Desktop/WHOI/Data/output_data/")

path_output = "~/Dropbox/data/outputs_Marthe_2023/heatmap_residence_time_polynya/OBS/"

dives = readRDS("dive_metrics_V3")
str(dives)

unique(dives$pol)
pal

refs = unique(dives$REF)[1:10]

list_file_deploy = list.files(path = "~/Dropbox/data/diag_dive_ind_pol/", pattern = '_diag.xlsx', recursive = TRUE)
list_deploy = o

# for (d in 1:100) {
#   if (365%%d == 0) {
#     print(d)
#   }
# }

for (deploy in list_deploy) {
  toto = subset(dives, subset = grepl(deploy, REF))
  n_seals = length(unique(toto$REF))
  # start_date = as.Date(as.POSIXct(paste0("01/01/", year(toto$DE_DATE[1])), tz = "UTC", format = "%d/%m/%Y"))
  # end_date = as.Date(as.POSIXct(paste0("31/12/", year(toto$DE_DATE[length(toto$DE_DATE)])), tz = "UTC", format = "%d/%m/%Y"))

  polynya_usage_deploy <- ggplot(data = toto) + 
    geom_tile(aes(x=yday(DE_DATE), y = REF, fill = as.factor(pol))) +
    scale_fill_manual(values = pal)+
    theme(axis.text.y = element_text(size = 20, face="bold"),
          axis.text.x = element_text(size = 20),
          axis.title = element_text(size = 25, face="bold"),
          legend.text = element_text(size=20), #__________ legend texts
          legend.title=element_text(size=20, face="bold"),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "white")
    )+ #__________ legend titles
    #xlim(start_date, end_date)+
    #xlim(0, 365)+
    scale_x_continuous(limits = c(0, 365), breaks = seq(0, 365, by = 30))+
    #theme_bw()+
    xlab("Time")+
    ylab("")+
    labs(fill = "Polynya id")
  
  pdf(paste0(path_output, "/by_deploy/", deploy, "_time_spend_in_pol.pdf"), height = n_seals*2, width = 15)
  print(polynya_usage_deploy)
  dev.off()
}



# full_timescale_HM <- ggplot(data = dives) + 
#   geom_tile(aes(x=as.Date(DE_DATE, format = "%b%Y"), y = REF, fill = as.factor(in_polynya))) +
#   scale_fill_viridis(discrete = T, direction = -1) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

polynya_usage_all_yday <- ggplot(data = dives) + 
  geom_tile(aes(x=yday(DE_DATE), y = REF, fill = as.factor(in_polynya))) +
  scale_fill_manual(values = pal_CESM_met)+
  #scale_fill_viridis(discrete = T, direction = -1) +
  theme(axis.text.y = element_text(size = 20, face="bold"),
        axis.text.x = element_text(size = 20),
        axis.title = element_text(size = 25, face="bold"),
        legend.text = element_text(size=20), #__________ legend texts
        legend.title=element_text(size=20, face="bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.1, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white")
        )+ #__________ legend titles
  scale_x_continuous(limits = c(0, 365), breaks = seq(0, 365, by = 30))+
  #theme_bw()+
  xlab("Day of the year")+
  ylab("")+
  labs(fill = "Polynya id")
 

n_seals = length(unique(dives$REF))

pdf(paste0(path_output, "all_seals_time_spend_in_pol_yday.pdf"), height = n_seals, width = 15)
print(polynya_usage_all_yday)
dev.off()


polynya_usage_all_FTS <- ggplot(data = dives) + 
  geom_tile(aes(x=as.Date(DE_DATE, format = "%d%b%Y"), y = REF, fill = as.factor(in_polynya))) +
  scale_fill_manual(values = pal_CESM_met)+
  #scale_fill_viridis(discrete = T, direction = -1) +
  theme(axis.text.y = element_text(size = 50, face="bold"),
        axis.text.x = element_text(size = 50),
        axis.title = element_text(size = 50, face="bold"),
        legend.text = element_text(size=50), #__________ legend texts
        legend.title=element_text(size=50, face="bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "grey",
                                        linewidth = 0.1, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = "grey")
  )+ #__________ legend titles
  #scale_x_continuous(limits = c(0, 365), breaks = seq(0, 365, by = 30))+
  #theme_bw()+
  xlab("Time")+
  ylab("")+
  labs(fill = "Polynya id")


n_seals = length(unique(dives$REF))

pdf(paste0(path_output, "all_seals_time_spend_in_pol_FTS.pdf"), height = n_seals*2, width = 100)
print(polynya_usage_all_FTS)
dev.off()


######## BY YEAR
years = unique(year(dives$DE_DATE))
for (year in years) {
  toto = subset(dives, subset = year(DE_DATE) == year)
  n_seals = length(unique(toto$REF))
  # start_date = as.Date(as.POSIXct(paste0("01/01/", year(toto$DE_DATE[1])), tz = "UTC", format = "%d/%m/%Y"))
  # end_date = as.Date(as.POSIXct(paste0("31/12/", year(toto$DE_DATE[length(toto$DE_DATE)])), tz = "UTC", format = "%d/%m/%Y"))
  
  polynya_usage_by_year <- ggplot(data = toto) + 
    geom_tile(aes(x=yday(DE_DATE), y = REF, fill = as.factor(pol))) +
    scale_fill_manual(values = pal_CESM_met)+
    #scale_fill_viridis(discrete = T, direction = -1) +
    theme(axis.text.y = element_text(size = 20, face="bold"),
          axis.text.x = element_text(size = 20),
          axis.title = element_text(size = 25, face="bold"),
          legend.text = element_text(size=20), #__________ legend texts
          legend.title=element_text(size=20, face="bold"),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "white")
    )+ #__________ legend titles
    #xlim(start_date, end_date)+
    #xlim(0, 365)+
    scale_x_continuous(limits = c(0, 365), breaks = seq(0, 365, by = 30))+
    #theme_bw()+
    xlab("Time")+
    ylab("")+
    labs(fill = "Polynya id")
  
  
  pdf(paste0(path_output, "/by_year/", year, "_time_spend_in_pol.pdf"), height = n_seals*2, width = 15)
  print(polynya_usage_by_year)
  dev.off()
}


######## BY POLYNYA
polynyas = unique(dives$pol)
polynyas = polynyas[-c(polynyas == 0)]

summary <- dives %>% 
  group_by(pol, day = yday(DE_DATE)) %>% 
  summarize(n_dives = n()) %>% 
  na.omit() %>% 
  mutate(freq = n_dives/sum(n_dives)) %>% 
  as.data.frame()

str(summary)

polynya_info <- read.csv("~/Desktop/WHOI/Data/polynyas_contours/OBS/ID_polynyas_Esther.csv", sep = ";")
polynya_info_dict <- as.list(polynya_info[,2])
names(polynya_info_dict) = polynya_info$ID


# apply(dives[row_dives_month, ], 1, function(dive) ifelse(is.na(dive[pol_i]),
#                                get_polynya_id(dive[lon_i], dive[lat_i], pol),
#                                dive[pol_i]))

summary_pol = subset(summary, pol > 0)
labels_pol = sapply(unique(summary_pol$pol), function(id) polynya_info_dict[[id]])

heatmap_residence_time_pol <-
  ggplot(summary_pol, aes(day, as.factor(pol))) + 
  geom_tile(aes(fill = n_dives)) +
  scale_fill_viridis_c() +
  scale_y_discrete(labels = labels_pol) +
  scale_x_continuous(limits = c(0, 365), breaks = seq(0, 365, by = 30)) +
  xlab("Day of the year") +
  ylab("") +
  labs(fill = "Number of dives") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 20, face="bold"),
        axis.text.x = element_text(size = 20),
        axis.title = element_text(size = 25, face="bold"),
        legend.text = element_text(size=20), #__________ legend texts
        legend.title=element_text(size=20, face="bold"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.05, linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white"))

ggsave(paste0(path_output, "RESIDENCE_TIME_polynyas.png"), height = 30, width = 50, units = c("cm"), dpi = 300)

for (pol_id in polynyas) {
  toto = subset(dives, subset = pol == pol_id)
  n_seals = length(unique(toto$REF))
  # start_date = as.Date(as.POSIXct(paste0("01/01/", year(toto$DE_DATE[1])), tz = "UTC", format = "%d/%m/%Y"))
  # end_date = as.Date(as.POSIXct(paste0("31/12/", year(toto$DE_DATE[length(toto$DE_DATE)])), tz = "UTC", format = "%d/%m/%Y"))
  
  residence_time_by_pol <- ggplot(data = toto) + 
    geom_tile(aes(x=yday(DE_DATE), y = REF, fill = as.factor(pol))) +
    scale_fill_manual(values = pal_CESM_met)+
    #scale_fill_viridis(discrete = T, direction = -1) +
    theme(axis.text.y = element_text(size = 20, face="bold"),
          axis.text.x = element_text(size = 20),
          axis.title = element_text(size = 25, face="bold"),
          legend.text = element_text(size=20), #__________ legend texts
          legend.title=element_text(size=20, face="bold"),
          panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          linewidth = 0.5, linetype = "solid"),
          panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                          colour = "white")
    )+ #__________ legend titles
    #xlim(start_date, end_date)+
    #xlim(0, 365)+
    scale_x_continuous(limits = c(0, 365), breaks = seq(0, 365, by = 30))+
    #theme_bw()+
    xlab("Time")+
    ylab("")+
    labs(fill = "Polynya id")
  
  
  pdf(paste0(path_output, "/by_year/", year, "_time_spend_in_pol.pdf"), height = n_seals*2, width = 15)
  print(polynya_usage_by_year)
  dev.off()
}
