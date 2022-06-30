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

# ICRS: keep only 9 cameras (6 LD and 3 SD):
dist_df <- dist_df[c(2, 4, 6, 7, 8, 9, 10, 11, 12), c(2, 4, 6, 7, 8, 9, 10, 11, 12)]


## 2 - Prepare data before computation of metrics across increasing timespan ####


# PREPARE DATA: clean abund list:
clean_abund_list <- abund_list

# ICRS: keep only 9 cameras:
for (i in (1:length(clean_abund_list))) {
  for (j in (1:length(clean_abund_list[[i]]))) {
    clean_abund_list[[i]][[j]] <- clean_abund_list[[i]][[j]][, which(colnames(clean_abund_list[[i]][[j]]) %in%
                                                                       c("A1", "A2", "B1", "B2",
                                                                         "C1", "C2", "D", "F", "H"))]
  }
}

abund_list <- clean_abund_list

# Get the timespans to work on:
spans_set <- c(600, 1200, 1800, 2400, 3000, 3600)

# get the names of cameras for which combinaisns must be computed:
# ICRS 9 cameras:
cam_set <- c("A1", "A2", "B1", "B2",
             "C1", "C2", "D", "F", "H")


## 3 - Compute maxN, SmaxN values for all timespan from 600 seconds to 3600 seconds ####


# Species 1 - Ac Cten dark

# create a list of abund df with all cam (even if the species ...
# ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# ... the list is build as follow:

# be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
abund_allcam_list_AcCten <- create.abundlist.allcam.poses(cam_set = cam_set,
                                                      species_nm = "Ac_Cten_dark",
                                                      abund_list = abund_list)


# create a list of abund df for timespan
abund_timespan_list_AcCten <- create.abund.list.timespan(spans_set = spans_set,
                                                     abund_allcam_list = abund_allcam_list_AcCten)


# create the df for plot (really long process so uncomment if want to run again):
maxN_timespan_AcCten <- compute.maxN.combcam(abund_combcam_list = abund_timespan_list_AcCten,
                                         dist_df = dist_df,
                                         fish_speed = 0.5,
                                         analysis_type = "timespan")

# save
saveRDS(maxN_timespan_AcCten, here::here("transformed_data","maxN_timespan_raw_AcCten.rds"))



# Species 2 - Gomphosus caeruleus

# create a list of abund df with all cam (even if the species ...
# ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# ... the list is build as follow:

# be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
abund_allcam_list_GC <- create.abundlist.allcam.poses(cam_set = cam_set,
                                                      species_nm = "Gomphosus_caeruleus",
                                                      abund_list = abund_list)


# create a list of abund df for timespan
abund_timespan_list_GC <- create.abund.list.timespan(spans_set = spans_set,
                                                    abund_allcam_list = abund_allcam_list_GC)


# create the df for plot (really long process so uncomment if want to run again):
maxN_timespan_GC <- compute.maxN.combcam(abund_combcam_list = abund_timespan_list_GC,
                                        dist_df = dist_df,
                                        fish_speed = 0.5,
                                        analysis_type = "timespan")

# save
saveRDS(maxN_timespan_GC, here::here("transformed_data", "maxN_timespan_raw_GC.rds"))


# Species 3 - Parupeneus macronemus

# create a list of abund df with all cam (even if the species ...
# ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# ... the list is build as follow:

# be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
abund_allcam_list_PM <- create.abundlist.allcam.poses(cam_set = cam_set,
                                                      species_nm = "Parupeneus_macronemus",
                                                      abund_list = abund_list)


# create a list of abund df for timespan
abund_timespan_list_PM <- create.abund.list.timespan(spans_set = spans_set,
                                                     abund_allcam_list = abund_allcam_list_PM)


# create the df for plot (really long process so uncomment if want to run again):
maxN_timespan_PM <- compute.maxN.combcam(abund_combcam_list = abund_timespan_list_PM,
                                         dist_df = dist_df,
                                         fish_speed = 0.5,
                                         analysis_type = "timespan")

# save
saveRDS(maxN_timespan_PM, here::here("transformed_data", "maxN_timespan_raw_PM.rds"))



# Species 4 - Chaetodon trifasciatus

# create a list of abund df with all cam (even if the species ...
# ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# ... the list is build as follow:

# be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
abund_allcam_list_CT <- create.abundlist.allcam.poses(cam_set = cam_set,
                                                      species_nm = "Chaetodon_trifasciatus",
                                                      abund_list = abund_list)


# create a list of abund df for timespan
abund_timespan_list_CT <- create.abund.list.timespan(spans_set = spans_set,
                                                     abund_allcam_list = abund_allcam_list_CT)


# create the df for plot (really long process so uncomment if want to run again):
maxN_timespan_CT <- compute.maxN.combcam(abund_combcam_list = abund_timespan_list_CT,
                                         dist_df = dist_df,
                                         fish_speed = 0.5,
                                         analysis_type = "timespan")

# save
saveRDS(maxN_timespan_CT, here::here("transformed_data","maxN_timespan_raw_CT.rds"))



# Species 5 - Thalassoma hardwicke

# create a list of abund df with all cam (even if the species ...
# ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# ... the list is build as follow:

# be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
abund_allcam_list_TH <- create.abundlist.allcam.poses(cam_set = cam_set,
                                                      species_nm = "Thalassoma_hardwicke",
                                                      abund_list = abund_list)


# create a list of abund df for timespan
abund_timespan_list_TH <- create.abund.list.timespan(spans_set = spans_set,
                                                     abund_allcam_list = abund_allcam_list_TH)


# create the df for plot (really long process so uncomment if want to run again):
maxN_timespan_TH <- compute.maxN.combcam(abund_combcam_list = abund_timespan_list_TH,
                                         dist_df = dist_df,
                                         fish_speed = 0.5,
                                         analysis_type = "timespan")

# save
saveRDS(maxN_timespan_TH, here::here("transformed_data", "maxN_timespan_raw_TH.rds"))



# Species 6 - Parapercis hexophtalma

# create a list of abund df with all cam (even if the species ...
# ... is not seen by several cam thus the abund df has 9 (ICRS) 12 (paper) columns).
# ... the list is build as follow:

# be careful for paper, change create.abundlist.allcam.poses() function because 12 cam
abund_allcam_list_PH <- create.abundlist.allcam.poses(cam_set = cam_set,
                                                      species_nm = "Parapercis_hexophtalma",
                                                      abund_list = abund_list)


# create a list of abund df for timespan
abund_timespan_list_PH <- create.abund.list.timespan(spans_set = spans_set,
                                                     abund_allcam_list = abund_allcam_list_PH)


# create the df for plot (really long process so uncomment if want to run again):
maxN_timespan_PH <- compute.maxN.combcam(abund_combcam_list = abund_timespan_list_PH,
                                         dist_df = dist_df,
                                         fish_speed = 0.5,
                                         analysis_type = "timespan")

# save
saveRDS(maxN_timespan_PH, here::here("transformed_data", "maxN_timespan_raw_PH.rds"))



# 3 - Plot ####


# call data:
SmaxN_timespan_AcCten <- readRDS(here::here("transformed_data", "maxN_timespan_raw_AcCten.rds"))
SmaxN_timespan_CT <- readRDS(here::here("transformed_data", "maxN_timespan_raw_CT.rds"))
SmaxN_timespan_GC <- readRDS(here::here("transformed_data", "maxN_timespan_raw_GC.rds"))
SmaxN_timespan_PM <- readRDS(here::here("transformed_data", "maxN_timespan_raw_PM.rds"))
SmaxN_timespan_PH <- readRDS(here::here("transformed_data", "maxN_timespan_raw_PH.rds"))
SmaxN_timespan_TH <- readRDS(here::here("transformed_data", "maxN_timespan_raw_TH.rds"))



# gather data for all species:
all_sp_list <- list(SmaxN_timespan_AcCten, SmaxN_timespan_CT, SmaxN_timespan_GC,
                    SmaxN_timespan_PM, SmaxN_timespan_PH, SmaxN_timespan_TH)
names(all_sp_list) <- c("AcCten_dark", "Chaetodon_trifasciatus",
                        "Gomphosus_caeruleus", "Parupeneus_macronemus",
                        "Parapercis_hexophtalma", "Thalassoma_hardwicke")


# prepare df to plot:
timespan_full_df <- clean.df.combcam.maxN(all_sp_list = all_sp_list,
                                          type = "timespan")
saveRDS(timespan_full_df, here::here("transformed_data", "final_timespan.rds"))


colors <- c("gray50", "#0ABFD6")
alpha <- c(1, 1)
shape <- c(21, 22, 24)
size <- c(3, 2)

# plot and save in trasformed data forlder:
plot_time <- timespans.plot(maxN_timespans = timespan_full_df,
                            colors = colors,
                            alpha = alpha,
                            shape_pose = shape,
                            size = size,
                            compare = "maxN")

