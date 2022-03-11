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

# Load distance data (dist between the cameras):
dist_df <- read.csv(here::here("data", "dist_df.csv"), row.names = 1)



## 2 - Prepare data for the plot ####


# Get the timespans to work on:
spans_set <- c(600, 1200, 1800, 2400, 3000, 3600)

# Get the list of abundance data with each element is a list for a given ...
# ... species of all abundance dataframes with more or less rows ie ...
# ... more or less minutes:
abund_time_list <- create.abund.list.timespan(spans_set = spans_set,
                                       abund_cam_allposes_list = abund_list)


# Compute maxN values for each timespan using combcam function because ...
# ... works the same :
maxN_timespans <- compute.maxN.combcam(abund_combcam_list = abund_time_list,
                                       dist_df = dist_df,
                                       fish_speed = 2,
                                       analysis_type = "timespan")



# 3 - Plot ####





