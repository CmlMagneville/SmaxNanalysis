###########################################################################
##
## Script to prepare data and plot the evolution of SmaxN (3h melted) for
## ... an increasing number of cameras
##
## 4_SmaxN_increasing_nb_cam_analysis.R
##
## 07/03/2022
##
## Camille Magneville
##
###########################################################################


## 0 - 1 Load data ####

abund_list <- readRDS(here::here("transformed_data", "all_abund_list.rds"))
dist_df <- read.csv(here::here("data", "dist_df.csv"), row.names = 1)



## 1 - Prepare data for plotting ####

# get the names of species which are of interest:
species_set <- c("Gomphosus_caeruleus", "Parupeneus_macronemus",
                 "Chaetodon_auriga", "Parapercis_hexophtalma",
                 "Chaetodon_trifasciatus", "Thalassoma_hardwicke",
                 "Oxymonacanthus_longirostris")

# get the names of cameras for which combinaisns must be computed:
cam_set <- c("A1", "A2", "B1", "B2", "C1", "C2",
             "D", "E", "F", "G", "H", "I")


# create a list of abund df with all poses fusioned in one df and ...
# ... 12 number of cameras (even if the species is not seen by several cam ...
# ... during a given pose, the abund df has 12 columns):

abund_cam_allposes_list <- create.abundlist.allcam.poses(cam_set = cam_set,
                                                   species_set = species_set,
                                                   abund_list = abund_list)


# create a list of abund df for each combination of the cameras ...
# ... with all poses fusionned in one df:
abund_combcam_allposes_list <- create.abund.list.camcombn(cam_set = cam_set,
                                       abund_cam_allposes_list = abund_cam_allposes_list )



## 2 - Plot ####
