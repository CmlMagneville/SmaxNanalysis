###########################################################################
##
## Script of analysis to study the effect of fish speed
##
## 6_Fish_speed_analysis.R
##
## 29/03/2022
##
## Camille Magneville
##
###########################################################################


## 0 - Load data ####

abund_list <- readRDS(here::here("transformed_data", "all_abund_list.rds"))
dist_df <- read.csv(here::here("data", "dist_df.csv"), row.names = 1)


## 1 - Prepare data ####

# get the names of species which are of interest:
species_set <- c("Gomphosus_caeruleus", "Parupeneus_macronemus",
                 "Chaetodon_auriga", "Parapercis_hexophtalma",
                 "Chaetodon_trifasciatus", "Thalassoma_hardwicke",
                 "Oxymonacanthus_longirostris")

# compute the df with all info for the plot with same fish speed for all species:
# ... note: if not same fish spped for all species, change the automat.maxN.setsp
# ... function for speed = 0.5 m/s:
maxN_all_df_speed1 <- automat.maxN.setsp(species_set = species_set,
                                  abund_list = abund_list,
                                  dist_df = dist_df,
                                  fish_speed = 0.5)

# compute the df with all info for the plot with same fish speed for all species:
# ... note: if not same fish spped for all species, change the automat.maxN.setsp
# ... function for speed = 1 m/s:
maxN_all_df_speed2 <- automat.maxN.setsp(species_set = species_set,
                                         abund_list = abund_list,
                                         dist_df = dist_df,
                                         fish_speed = 1)


## 2 - Plot ####


maxN_speed1 <- maxN_all_df_speed1
maxN_speed2 <- maxN_all_df_speed2
color_poses <- c("#66c2a5", "#fc8d62", "#8da0cb")
alpha <- 0.8
size <- 2.5
shape <- 22



fish_speed <- speed.plot2(maxN_speed1 = maxN_all_df_speed1, maxN_speed2 = maxN_all_df_speed2,
           color_poses = color_poses,
           alpha = alpha,
           size = size,
           shape = shape)

