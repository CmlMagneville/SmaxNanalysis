###########################################################################
##
## Script of analysis to study the effect of fish speed
##
## 6_Fish_speed_analysis.R
##
## 29/03/2022 23/06/2022
##
## Camille Magneville
##
###########################################################################


## 0 - Load data ####
abund_list <- readRDS(here::here("transformed_data", "all_abund_list.rds"))
dist_df <- read.csv(here::here("data", "dist_df.csv"), row.names = 1)

# ICRS: keep only 9 cameras (6 LD and 3 SD):
dist_df <- dist_df[c(2, 4, 6, 7, 8, 9, 10, 11, 12), c(2, 4, 6, 7, 8, 9, 10, 11, 12)]


## 1 - Prepare data ####


# PREPARE DATA
clean_abund_list <- abund_list

# ICRS: keep only 9 cameras:
for (i in (1:length(clean_abund_list))) {
  for (j in (1:length(clean_abund_list[[i]]))) {
    clean_abund_list[[i]][[j]] <- clean_abund_list[[i]][[j]][, which(colnames(clean_abund_list[[i]][[j]]) %in%
                                                                       c("A1", "A2", "B1", "B2",
                                                                         "C1", "C2", "D", "F", "H"))]
  }
}

# create a dataframe which will contain maxN values for all sp:
maxN_all <- as.data.frame(matrix(ncol = 5, nrow = 1))
colnames(maxN_all) <- c("species_nm", "pose_nb", "maxN", "SmaxN", "SmaxN_timestep")


## 2 - Compute SmaxN values for each species: code commented as long to run and no need to again ####



## Species 1: Ac_Cten_Dark:

# start <- Sys.time()
# SmaxN_AcCten <- automat.maxN.spbysp(species_nm = "Ac_Cten_dark",
#                                     abund_list = clean_abund_list,
#                                     dist_df = dist_df,
#                                     fish_speed = 1,
#                                     os = "windows",
#                                     nb_cores = 3)
# stop <- Sys.time()
# time_taken_AcCten <- stop - start
# saveRDS(SmaxN_AcCten, here::here("transformed_data", "SmaxN_AcCten_raw_speed2.rds"))

# ... h?



## Species 2: Gomphosus_caeruleus: j'ai essaye en parallelisant:
# start <- Sys.time()
# SmaxN_GC <- automat.maxN.spbysp(species_nm = "Gomphosus_caeruleus",
#                                 abund_list = clean_abund_list,
#                                 dist_df = dist_df,
#                                 fish_speed = 1,
#                                 os = "windows",
#                                 nb_cores = 3)
# stop <- Sys.time()
# time_taken_GC <- stop - start
# saveRDS(SmaxN_GC, here::here("transformed_data", "SmaxN_GC_raw_speed2.rds"))

# 3.78 heures


## Species 3 - Chaetodon_trifasciatus

# start <- Sys.time()
# SmaxN_CT <- automat.maxN.spbysp(species_nm = "Chaetodon_trifasciatus",
#                                 abund_list = clean_abund_list,
#                                 dist_df = dist_df,
#                                 fish_speed = 1,
#                                 os = "windows",
#                                 nb_cores = 3)
# stop <- Sys.time()
# time_taken_CT <- stop - start
# saveRDS(SmaxN_CT, here::here("transformed_data", "SmaxN_CT_raw_speed2.rds"))

# 5 min


## Species 4 - Parupeneus_macronemus: "Parupeneus_macronemus"

# start <- Sys.time()
# SmaxN_PM <- automat.maxN.spbysp(species_nm = "Parupeneus_macronemus",
#                                 abund_list = clean_abund_list,
#                                 dist_df = dist_df,
#                                 fish_speed = 1,
#                                 os = "windows",
#                                 nb_cores = 3)
# stop <- Sys.time()
# time_taken_PM <- stop - start
# saveRDS(SmaxN_PM, here::here("transformed_data", "SmaxN_PM_raw_speed2.rds"))

# 19 min


## Species 5 - Thalassoma hardwicke:

# start <- Sys.time()
# SmaxN_TH <- automat.maxN.spbysp(species_nm = "Thalassoma_hardwicke",
#                                 abund_list = clean_abund_list,
#                                 dist_df = dist_df,
#                                 fish_speed = 1,
#                                 os = "windows",
#                                 nb_cores = 3)
# stop <- Sys.time()
# time_taken_TH <- stop - start
# saveRDS(SmaxN_TH, here::here("transformed_data", "SmaxN_TH_raw_speed2.rds"))


## Species 6 - Parapercis hexophtalma:

# start <- Sys.time()
# SmaxN_PH <- automat.maxN.spbysp(species_nm = "Parapercis_hexophtalma",
#                                 abund_list = clean_abund_list,
#                                 dist_df = dist_df,
#                                 fish_speed = 1,
#                                 os = "windows",
#                                 nb_cores = 3)
# stop <- Sys.time()
# time_taken_PH <- stop - start
# saveRDS(SmaxN_PH, here::here("transformed_data", "SmaxN_PH_raw_speed2.rds"))

## 2 - Plot ####


# call data speed 0.5:
SmaxN_AcCten <- readRDS(here::here("transformed_data", "SmaxN_AcCten_raw.rds"))
SmaxN_CT <- readRDS(here::here("transformed_data", "SmaxN_CT_raw.rds"))
SmaxN_GC <- readRDS(here::here("transformed_data", "SmaxN_GC_raw.rds"))
SmaxN_PM <- readRDS(here::here("transformed_data", "SmaxN_PM_raw.rds"))
SmaxN_TH <- readRDS(here::here("transformed_data", "SmaxN_TH_raw.rds"))
SmaxN_PH <- readRDS(here::here("transformed_data", "SmaxN_PH_raw.rds"))

# call data speed 1:
SmaxN_AcCten_speed2 <- readRDS(here::here("transformed_data", "SmaxN_AcCten_raw_speed2.rds"))
SmaxN_CT_speed2 <- readRDS(here::here("transformed_data", "SmaxN_CT_raw_speed2.rds"))
SmaxN_GC_speed2 <- readRDS(here::here("transformed_data", "SmaxN_GC_raw_speed2.rds"))
SmaxN_PM_speed2 <- readRDS(here::here("transformed_data", "SmaxN_PM_raw_speed2.rds"))
SmaxN_TH_speed2 <- readRDS(here::here("transformed_data", "SmaxN_TH_raw_speed2.rds"))
SmaxN_PH_speed2 <- readRDS(here::here("transformed_data", "SmaxN_PH_raw_speed2.rds"))


# gather data for all species - speed 0.5m/s:
all_sp_list <- list(SmaxN_AcCten, SmaxN_CT, SmaxN_GC, SmaxN_PH, SmaxN_PM, SmaxN_TH)
names(all_sp_list) <- c("AcCten_dark", "Chaetodon_trifasciatus",
                        "Gomphosus_caeruleus", "Parapercis_hexophtalma",
                        "Parupeneus_macronemus", "Thalassoma_hardwicke")


# gather data for all species - speed 1m/s:
all_sp_list_speed2 <- list(SmaxN_AcCten_speed2, SmaxN_CT_speed2, SmaxN_GC_speed2, SmaxN_PH_speed2,
                    SmaxN_PM_speed2, SmaxN_TH_speed2)
names(all_sp_list_speed2) <- c("AcCten_dark", "Chaetodon_trifasciatus",
                        "Gomphosus_caeruleus", "Parapercis_hexophtalma",
                        "Parupeneus_macronemus", "Thalassoma_hardwicke")


# to a clean table - speed 0.5m/s:
SmaxN_df_all_sp <- clean.df.maxN(all_sp_list = all_sp_list)

# correct because bug for AcCten_dark, using wrong df but got data through combcam and check:
# so I correct manually while the SmaxN_ActCten rerun:
SmaxN_df_all_sp$SmaxN[c(1,2,3)] <- c(16,16,15)
SmaxN_df_all_sp$maxN[c(1,2,3)] <- c(4,4,5)
SmaxN_df_all_sp$SmaxN_timestep[c(1,2,3)] <- c(10,10,10)

saveRDS(SmaxN_df_all_sp, here::here("transformed_data", "SmaxN_df_all_sp.rds"))



# to a clean table - speed 1m/s:
SmaxN_df_all_sp_speed2 <- clean.df.maxN(all_sp_list = all_sp_list_speed2)
saveRDS(SmaxN_df_all_sp_speed2, here::here("transformed_data", "SmaxN_df_all_sp_speed2.rds"))




maxN_speed1 <- SmaxN_df_all_sp
maxN_speed2 <- SmaxN_df_all_sp_speed2

color_poses <- c("#66c2a5", "#fc8d62", "#8da0cb")
alpha <- 0.8
size <- 2.5
shape <- 21



fish_speed <- speed.plot2(maxN_speed1 = maxN_all_df_speed1, maxN_speed2 = maxN_all_df_speed2,
           color_poses = color_poses,
           alpha = alpha,
           size = size,
           shape = shape)

