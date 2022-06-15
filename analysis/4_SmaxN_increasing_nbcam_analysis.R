###########################################################################
##
## Script to prepare data and plot the evolution of SmaxN (3h melted) for
## ... an increasing number of cameras
##
## 4_SmaxN_increasing_nb_cam_analysis.R
##
## 07/03/2022 puis 10/06/2022
##
## Camille Magneville
##
###########################################################################


## 1 Load data ####

abund_list <- readRDS(here::here("transformed_data", "all_abund_list.rds"))
dist_df <- read.csv(here::here("data", "dist_df.csv"), row.names = 1)

# ICRS: keep only 9 cameras (6 LD and 3 SD):
dist_df <- dist_df[c(2, 4, 6, 7, 8, 9, 10, 11, 12), c(2, 4, 6, 7, 8, 9, 10, 11, 12)]


## 2 - Compute maxN, SmaxN values for all combinations of cameras and 4 species ####


# # get the names of species which are of interest:
# species_set <- c("Gomphosus_caeruleus", "Parupeneus_macronemus",
#                  "Chaetodon_auriga", "Parapercis_hexophtalma",
#                  "Chaetodon_trifasciatus", "Thalassoma_hardwicke",
#                  "Oxymonacanthus_longirostris", "Ac_Cten_dark")
#
# # get the names of cameras for which combinaisns must be computed:
# # ICRS 9 cameras:
# cam_set <- c("A1", "A2", "B1", "B2",
#              "C1", "C2", "D", "F", "H")
#
#
#
# # PREPARE DATA
# # restrict the abund_list to the studied species:
# clean_abund_list <- abund_list[which(names(abund_list) %in% species_set)]
#
# # ICRS: keep only 9 cameras:
# for (i in (1:length(clean_abund_list))) {
#   for (j in (1:length(clean_abund_list[[i]]))) {
#     clean_abund_list[[i]][[j]] <- clean_abund_list[[i]][[j]][, which(colnames(clean_abund_list[[i]][[j]]) %in%
#                                                                        c("A1", "A2", "B1", "B2",
#                                                                          "C1", "C2", "D", "F", "H"))]
#   }
# }
#
# abund_list <- clean_abund_list
#
#
# ## Species 1 - AcCten_dark
#
#
# # create a list of abund df with all cam (even if the species ...
# # ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# # ... the list is build as follow:
#
# # be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
# abund_allcam_list_AcCten <- create.abundlist.allcam.poses(cam_set = cam_set,
#                                                           species_nm = "Ac_Cten_dark",
#                                                           abund_list = abund_list)
#
#
# # create a list of abund df for each combination of the cameras BUT ...
# # ... : But this list is to heavy to be saved on Github so must run ...
# # ... this step and the next one only once and the dataframe from the next ...
# # ... step will be saved.
# abund_combcam_list_AcCten <- create.abund.list.camcombn(cam_set = cam_set,
#                                                         abund_allcam_list = abund_allcam_list_AcCten)
#
# # create the df for plot (really long process so uncomment if want to run again):
# maxN_combcam_AcCten <- compute.maxN.combcam(abund_combcam_list = abund_combcam_list_AcCten,
#                                             dist_df = dist_df,
#                                             fish_speed = 0.5,
#                                             analysis_type = "combcam")
#
# # save
# saveRDS(maxN_combcam_AcCten, "maxN_combcam_raw_AcCten.rds")
#
#
# ## Species 2 - Gomphosus caeruleus
#
# # create a list of abund df with all cam (even if the species ...
# # ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# # ... the list is build as follow:
#
# # be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
# abund_allcam_list_GC <- create.abundlist.allcam.poses(cam_set = cam_set,
#                                                       species_nm = "Gomphosus_caeruleus",
#                                                       abund_list = abund_list)
#
#
# # create a list of abund df for each combination of the cameras BUT ...
# # ... : But this list is to heavy to be saved on Github so must run ...
# # ... this step and the next one only once and the dataframe from the next ...
# # ... step will be saved.
# abund_combcam_list_GC <- create.abund.list.camcombn(cam_set = cam_set,
#                                                     abund_allcam_list = abund_allcam_list_GC)
#
# # create the df for plot (really long process so uncomment if want to run again):
# maxN_combcam_GC <- compute.maxN.combcam(abund_combcam_list = abund_combcam_list_GC,
#                                         dist_df = dist_df,
#                                         fish_speed = 0.5,
#                                         analysis_type = "combcam")
#
# # save
# saveRDS(maxN_combcam_GC, "maxN_combcam_raw_GC.rds")
#
#
# ## Species 3 - Parupeneus macronemus
#
#
#
# # create a list of abund df with all cam (even if the species ...
# # ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# # ... the list is build as follow:
#
# # be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
# abund_allcam_list_PM <- create.abundlist.allcam.poses(cam_set = cam_set,
#                                                       species_nm = "Parupeneus_macronemus",
#                                                       abund_list = abund_list)
#
#
# # create a list of abund df for each combination of the cameras BUT ...
# # ... : But this list is to heavy to be saved on Github so must run ...
# # ... this step and the next one only once and the dataframe from the next ...
# # ... step will be saved.
# abund_combcam_list_PM <- create.abund.list.camcombn(cam_set = cam_set,
#                                                     abund_allcam_list = abund_allcam_list_PM)
#
# # create the df for plot (really long process so uncomment if want to run again):
# maxN_combcam_PM <- compute.maxN.combcam(abund_combcam_list = abund_combcam_list_PM,
#                                         dist_df = dist_df,
#                                         fish_speed = 0.5,
#                                         analysis_type = "combcam")
#
# # save
# saveRDS(maxN_combcam_PM, "maxN_combcam_raw_PM.rds")
#
#
#
# ## Species 4 - Chaetodon trifasciatus
#
# # create a list of abund df with all cam (even if the species ...
# # ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# # ... the list is build as follow:
#
# # be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
# abund_allcam_list_CT <- create.abundlist.allcam.poses(cam_set = cam_set,
#                                                    species_nm = "Chaetodon_trifasciatus",
#                                                    abund_list = abund_list)
#
#
# # create a list of abund df for each combination of the cameras BUT ...
# # ... : But this list is to heavy to be saved on Github so must run ...
# # ... this step and the next one only once and the dataframe from the next ...
# # ... step will be saved.
# abund_combcam_list_CT <- create.abund.list.camcombn(cam_set = cam_set,
#                                                  abund_allcam_list = abund_allcam_list_CT)
#
# # create the df for plot (really long process so uncomment if want to run again):
# maxN_combcam_CT <- compute.maxN.combcam(abund_combcam_list = abund_combcam_list_CT,
#                                         dist_df = dist_df,
#                                         fish_speed = 0.5,
#                                         analysis_type = "combcam")
# saveRDS(maxN_combcam_CT, "maxN_combcam_raw_CT.rds")



## 3 - Arrange data for plotting ####


# call data:
SmaxN_combcam_AcCten <- readRDS(here::here("transformed_data", "maxN_combcam_raw_AcCten.rds"))
SmaxN_combcam_CT <- readRDS(here::here("transformed_data", "maxN_combcam_raw_CT.rds"))
SmaxN_combcam_GC <- readRDS(here::here("transformed_data", "maxN_combcam_raw_GC.rds"))
SmaxN_combcam_PM <- readRDS(here::here("transformed_data", "maxN_combcam_raw_PM.rds"))


# gather data for all species:
all_sp_list <- list(SmaxN_combcam_AcCten, SmaxN_combcam_CT, SmaxN_combcam_GC,
                    SmaxN_combcam_PM)
names(all_sp_list) <- c("AcCten_dark", "Chaetodon_trifasciatus",
                        "Gomphosus_caeruleus", "Parupeneus_macronemus")


# prepare df to plot:
combcam_full_df <- clean.df.combcam.maxN(all_sp_list)
saveRDS(combcam_full_df, here::here("transformed_data", "final_combcam.rds"))


## 4 - Plot ####


## Plot:
colors <- c("gray78", "#66c2a5")
alpha <- c(1, 1)
shape <- c(22, 21)
size <- c(2, 1)

combcam_plot <- combcam.plot(maxN_combcam = comb_cam_full_df,
                             colors = colors,
                             alpha = alpha,
                             shape = shape,
                             size = size)
