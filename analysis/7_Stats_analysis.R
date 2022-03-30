###########################################################################
##
## Script of statistical analysis
##
## 7_Stats_analysis.R
##
## 30/03/2022
##
## Camille Magneville
##
###########################################################################



# 1 - Test significant differences between SmaxN and maxN (and SmaxN_row) for all species ####

# Load data
maxN_all <- readRDS(here::here("transformed_data", "maxN_all.rds"))

# param:
maxN_all <- maxN_all
colors_poses <- c("#66c2a5", "#fc8d62", "#8da0cb")
comp_metric <- "SmaxN_row"

# call fct to do the plot and the analysis:
cor.SmaxN.plot(maxN_all = maxN_all,
               colors_poses = colors_poses,
               comp_metric = comp_metric)



# 2 - Test significant differences of SmaxN with an increasing number of cameras ####


# Load data:
maxN_comb_cam <- readRDS(here::here("transformed_data", "maxN_combcam.rds"))

# Param:
SmaxN_df <- maxN_comb_cam
metric <- "cam_nb"

# plot:
kruskal.SmaxN.plot(SmaxN_df, metric)



# 3 - Test significant differences of SmaxN with an increasing recording time ####


# Load data:
maxN_timespan <- readRDS(here::here("transformed_data", "maxN_timespans.rds"))

# Param:
SmaxN_df <- maxN_timespan
metric <- "timespan"

# plot:
kruskal.SmaxN.plot(SmaxN_df, metric)




