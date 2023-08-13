rm(list=ls())

setwd("~/Desktop/WHOI/Data/")
dives <- readRDS("behavioural_data/dive_metrics_bottime_speed_interp_hunttime_bathy_zone_mode_pol_inpol_ctd_9") %>%
  filter(pol > 0)

library(corrplot)


library(dplyr)
dat <- dives %>%
  dplyr::select(c(DIVE_DUR, BOTT_TIME, MAX_DEP, hunting_time, bathy)) %>%
  na.omit()


test <- cor.test(dat$MAX_DEP, dat$mean_depth_HS)
test

pairs(dat[, c("bathy", "MAX_DEP")])


## Hunting segments table
HS <- readRDS("behavioural_data/hunting_time_segments")

## Add time, polynya id, dive mode and zone to hunting segments table
HS <- HS %>% left_join(dives, by = c("REF", "NUM")) #%>%
  na.omit()
  
  ## MLD
  ctd <- readRDS("ctd_data/ctd_stations_table_north_bound_interp_MLD") %>%
    dplyr::select(c(id_ctd, MLD))
  
  HS <- HS %>% left_join(ctd, by = "id_ctd")

  
  dat <- HS %>%
    dplyr::select(c(DIVE_DUR, BOTT_TIME, MAX_DEP, hunting_time, bathy, mean_depth_HS, id_ctd, MLD)) %>%
    na.omit()

  corrplot(cor(dat),
           method = "number",
           type = "upper" # show only upper side
  )
  
  
  library(Hmisc)
  res <- rcorr(as.matrix(dat)) # rcorr() accepts matrices only
  
  # display p-values (rounded to 3 decimals)
  round(res$P, 3)
  
  
  ## MLD
  ctd <- readRDS("ctd_data/ctd_stations_table_north_bound_interp_MLD") %>%
    dplyr::select(c(id_ctd, MLD))

  dat <- dat %>% left_join(ctd, by = "id_ctd")
  