##
## Script name: workflow_data_SES.R
##
## Purpose of script:
##
## Author: Marthe Vienne
## Modified by:
##
## Date Created: 2023-05-17
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

## Working directory
setwd("~/Desktop/WHOI/Codes/")

## Step 1: compile ARGOS et dive files
source("compile_diag_files.R")
source("compile_dive_files.R")

## Step 2: prepare ARGOS data for SSM (according to Ian Jonsen) 
source("prep_data_ARGOS_bssm.R")

## Step 3: run BSSM 
source("run_bssm.R")

## Step 4: prepare dive metrics -> filtering process on dives + compute new metrics + interpolate locations with BSSM
## /!\ check filter_predicted_SSM
source("prep_dives_summary.R")

## Step 5: create dive table (for profiles)
source("compute_dives_table.R")

## Step 6: extract bathy under dives
source("extract_bathy_dives.R")

## Step 6: identify polynyas
#source("polynya_contours.R")

## Step 7: assign polynya id to dive
# source("assign_polynya_id_to_dive.R")

## End script
rm(list=ls())
