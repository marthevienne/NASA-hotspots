##
## Script name: 
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-06-13
## Date Modified:
##
## Email: marthevienne@hotmail.fr
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
rm(list=ls())
## ---------------------------
## Library
## ---------------------------

source("~/Desktop/WHOI/Codes/update_seals_table.R")

seals <- read.csv("~/Desktop/WHOI/Data/output_data/seals.csv")
dives <- readRDS("~/Desktop/WHOI/Data/output_data/dive_metrics_V7")

seals_in_pol <- dives %>%
  group_by(REF) %>%
  filter(pol > 0) %>%
  summarise(n = n()) %>%
  select(REF) %>%
  mutate(visit_polynya = "yes")

seals <- seals %>% left_join(seals_in_pol, by = "REF")

seals$visit_polynya[is.na(seals$visit_polynya) & seals$has_dives == 1] = "no" #___NA for tracks with no dives

## Remove seal tracks where seals don't visit polynyas
tracks <- seals %>% 
  filter(visit_polynya == "no") %>%
  pull(REF)

new_dives <- dives %>%
  filter(!(REF %in% tracks))

nrow(new_dives) / nrow(dives) * 100 #___95.8 %

seals <- update_seals_table(new_dives, seals, "n_dives_visit_polynya")

## Save seals table and dives table
saveRDS(new_dives, "~/Desktop/WHOI/Data/output_data/dive_metrics_V8")
write.csv(seals, "~/Desktop/WHOI/Data/output_data/seals.csv", row.names = F)

## End script
rm(list=ls())
