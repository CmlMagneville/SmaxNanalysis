###########################################################################
##
## Script to launch on the server to compute SmaxN and other metrics for
## ... a given species and combination of cameras: Gomphosus caeruleus
##
## Serveur_combcam_sp2.R
##
## 10/06/2022
##
## Camille Magneville
##
###########################################################################


## 0 - 1 Load data ####

abund_list <- readRDS(here::here("transformed_data", "all_abund_list.rds"))
dist_df <- read.csv(here::here("data", "dist_df.csv"), row.names = 1)

# ICRS: keep only 9 cameras (6 LD and 3 SD):
dist_df <- dist_df[c(2, 4, 6, 7, 8, 9, 10, 11, 12), c(2, 4, 6, 7, 8, 9, 10, 11, 12)]

# call the functions needed:
source(here::here("R", "6_SmaxN_across_camnb_functions.R"))
source(here::here("R", "3_Compute_combinaisons_functions.R"))


# load SmaxN pkge:
remotes::install_github("CmlMagneville/SmaxN", build_vignettes = TRUE, force = TRUE)


## 1 - Prepare data for computing ####

# get the names of species which are of interest:
species_set <- c("Gomphosus_caeruleus", "Parupeneus_macronemus",
                 "Chaetodon_auriga", "Parapercis_hexophtalma",
                 "Chaetodon_trifasciatus", "Thalassoma_hardwicke",
                 "Oxymonacanthus_longirostris", "Ac_Cten_dark")

# get the names of cameras for which combinaisns must be computed:
# ICRS 9 cameras:
cam_set <- c("A1", "A2", "B1", "B2",
             "C1", "C2", "D", "F", "H")



# PREPARE DATA
# restrict the abund_list to the studied species:
clean_abund_list <- abund_list[which(names(abund_list) %in% species_set)]

# ICRS: keep only 9 cameras:
for (i in (1:length(clean_abund_list))) {
  for (j in (1:length(clean_abund_list[[i]]))) {
    clean_abund_list[[i]][[j]] <- clean_abund_list[[i]][[j]][, which(colnames(clean_abund_list[[i]][[j]]) %in%
                                                                       c("A1", "A2", "B1", "B2",
                                                                         "C1", "C2", "D", "F", "H"))]
  }
}

abund_list <- clean_abund_list


## 2 - Compute SmaxN for all combinations of cameras

# create a list of abund df with all cam (even if the species ...
# ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# ... the list is build as follow:

# be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
abund_allcam_list_GC <- create.abundlist.allcam.poses(cam_set = cam_set,
                                                          species_nm = "Gomphosus_caeruleus",
                                                          abund_list = abund_list)


# create a list of abund df for each combination of the cameras BUT ...
# ... : But this list is to heavy to be saved on Github so must run ...
# ... this step and the next one only once and the dataframe from the next ...
# ... step will be saved.
abund_combcam_list_GC <- create.abund.list.camcombn(cam_set = cam_set,
                                                        abund_allcam_list = abund_allcam_list_GC)

# create the df for plot (really long process so uncomment if want to run again):
maxN_combcam_GC <- compute.maxN.combcam(abund_combcam_list = abund_combcam_list_GC,
                                            dist_df = dist_df,
                                            fish_speed = 0.5,
                                            analysis_type = "combcam")

# save
saveRDS(maxN_combcam_GC, "maxN_combcam_raw_GC.rds")
