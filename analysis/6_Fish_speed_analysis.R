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
# ... function for speed = 1 m/s:
maxN_all_df_speed1 <- automat.maxN.setsp(species_set = species_set,
                                  abund_list = abund_list,
                                  dist_df = dist_df,
                                  fish_speed = 1)

# compute the df with all info for the plot with same fish speed for all species:
# ... note: if not same fish spped for all species, change the automat.maxN.setsp
# ... function for speed = 1 m/s:
maxN_all_df_speed2 <- automat.maxN.setsp(species_set = species_set,
                                         abund_list = abund_list,
                                         dist_df = dist_df,
                                         fish_speed = 2)


## 2 - Plot ####


maxN_speed1 <- maxN_all_df_speed1
maxN_speed2 <- maxN_all_df_speed2
color_sp <- c("#8c510a", "#bf812d", "#dfc27d", "#c7eae5", "#80cdc1",
             "#35978f", "#01665e")
alpha <- 0.8
size <- 2.5
shape_pose <- c(22, 21, 24)



fish_speed <- speed.plot(maxN_speed1 = maxN_all_df_speed1, maxN_speed2 = maxN_all_df_speed2,
           color_sp = color_sp,
           alpha = alpha,
           size = size,
           shape_pose = shape_pose)

