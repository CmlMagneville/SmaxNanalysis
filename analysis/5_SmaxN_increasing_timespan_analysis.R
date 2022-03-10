###########################################################################
##
## Script to prepare data and plot the evolution of SmaxN (12 cam melted) for
## ... an increasing number of time
##
## 5_SmaxN_increasing_timespan_analysis.R
##
## 07/03/2022
##
## Camille Magneville
##
###########################################################################



## 1 - Load data ####

# Load abundance data all poses melted:
abund_list <- readRDS(here::here("transformed_data", "abund_list_fusionposes_allcam.rds"))



# 2 - Prepare data for the plot ####

# Get the timespans to work on:
spans_set <- c(600, 1200, 1800, 2400, 3000, 3600)


# 3 - Plot ####



