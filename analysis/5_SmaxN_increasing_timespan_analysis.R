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

# abundance data:
abund_list <- readRDS(here::here("transformed_data", "all_abund_list.rds"))

# Load distance data (dist between the cameras):
dist_df <- read.csv(here::here("data", "dist_df.csv"), row.names = 1)



## 2 - Prepare data for the plot ####


# Get the timespans to work on:
spans_set <- c(600, 1200, 1800, 2400, 3000, 3600)

# get the names of species which are of interest:
species_set <- c("Gomphosus_caeruleus", "Parupeneus_macronemus",
                 "Chaetodon_auriga", "Parapercis_hexophtalma",
                 "Chaetodon_trifasciatus", "Thalassoma_hardwicke",
                 "Oxymonacanthus_longirostris")

# get the names of cameras for which combinaisns must be computed:
cam_set <- c("A1", "A2", "B1", "B2", "C1", "C2",
             "D", "E", "F", "G", "H", "I")


# Get the list of abundance df for each pose and each species with all cam:
abund_allcam_list <- create.abundlist.allcam.poses(cam_set = cam_set,
                                                   species_set = species_set,
                                                   abund_list = abund_list)


# Get the list of abundance data with each element is a list for a given ...
# ... species of all abundance dataframes with more or less rows ie ...
# ... more or less minutes:
abund_time_list <- create.abund.list.timespan(spans_set = spans_set,
                                       abund_allcam_list = abund_allcam_list)

# Compute maxN values for each timespan using combcam function because ...
# ... works the same :
maxN_timespans <-  final.combcam(abund_combcam_list = abund_time_list,
                                 dist_df = dist_df,
                                 fish_speed = 1,
                                 analysis_type = "timespan")




# 3 - Plot ####

colors <- c("gray70", "#66c2a5")
alpha <- c(0.7, 0.7)
shape_pose <- c(22, 21, 24)
size <- c(2, 2)

# plot and save in trasformed data forlder:
plot_time <- timespans.plot(maxN_timespans = maxN_timespans,
                            colors = colors,
                            alpha = alpha,
                            shape_pose = shape_pose,
                            size = size)

