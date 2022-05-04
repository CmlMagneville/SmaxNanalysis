###########################################################################
##
## Script to prepare data and plot the deltas between SmaxN and maxN and between
## ... SmaxN_row and SmaxN
##
## 3_maxN_values_delta_analysis.R
##
## 07/03/2022
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
                 "Oxymonacanthus_longirostris", "Ac_Cten_dark")
                 #  , "Naso_brevirostris",
                 # "Sufflamen_chrysopterum", "Canthigaster_bennetti",
                 # "Cephalopholis_argus", "Cetoscarus_ocellatus",
                 # "Chaetodon_trifascialis", "Chlorurus_sordidus",
                 # "Chromis_weberi", "Zebrasoma_scopas", "Caranx_melampygus")

# compute the df with all info for the plot with same fish speed for all species:
# ... note: if not same fish spped for all species, change the automat.maxN.setsp
# ... function
maxN_all_df <- automat.maxN.setsp(species_set = species_set,
                                  abund_list = abund_list,
                                  dist_df = dist_df,
                                  fish_speed = 1)

# save the df:
saveRDS(maxN_all_df, here::here("transformed_data", "maxN_all.rds"))



## 2 - Plot

plot_deltas <- deltas.plot(maxN_all_df, colors = c("#66c2a5", "#fc8d62",
                                                   "#8da0cb"))

