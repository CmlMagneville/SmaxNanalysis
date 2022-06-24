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



## Species 1: Ac_Cten_Dark: j'ai essaye pose apres pose:

# Pose 1 voir si ça marche:
abund_df <- paral_list[[1]]$abund_df
dist_df <- paral_list[[1]]$dist_df
speed <- paral_list[[1]]$speed

#start <- Sys.time()
# Ac_Cten_P1 <- SmaxN::SmaxN.computation(abund_df, speed, dist_df)
# stop <- Sys.time()
# time_taken_Ac_Cten_P1 <- stop - start

# ICRS 9 cameras prend 3min30


# Pose 2:
abund_df <- paral_list[[2]]$abund_df
dist_df <- paral_list[[2]]$dist_df
speed <- paral_list[[2]]$speed

# start <- Sys.time()
# Ac_Cten_P2 <- SmaxN::SmaxN.computation(abund_df, speed, dist_df)
# stop <- Sys.time()
# time_taken_Ac_Cten_P2 <- stop - start

# ICRS avec 9 cameras: prend: 3h30


# Pose 3:
abund_df <- paral_list[[3]]$abund_df
dist_df <- paral_list[[3]]$dist_df
speed <- paral_list[[3]]$speed

# start <- Sys.time()
# Ac_Cten_P3 <- SmaxN::SmaxN.computation(abund_df, speed, dist_df)
# stop <- Sys.time()
# time_taken_Ac_Cten_P3 <- stop - start
# saveRDS(Ac_Cten_P3, here::here("transformed_data", "Ac_Cten_P3.rds"))

# ICRS 9 camera prend 12 minutes

# combine all of them for ICRS:
# SmaxN_AcCten <- list(Ac_Cten_P1, Ac_Cten_P2, Ac_Cten_P3)


# essaye en parallelisant:
start <- Sys.time()
SmaxN_AcCten <- automat.maxN.spbysp(species_nm = "Ac_Cten_dark",
                                    abund_list = clean_abund_list,
                                    dist_df = dist_df,
                                    fish_speed = 0.5,
                                    os = "windows",
                                    nb_cores = 3)
stop <- Sys.time()
time_taken_AcCten <- stop - start
saveRDS(SmaxN_AcCten, here::here("transformed_data", "SmaxN_AcCten_raw.rds"))

# ... h?



## Species 2: Gomphosus_caeruleus: j'ai essaye en parallelisant:
# start <- Sys.time()
# SmaxN_GC <- automat.maxN.spbysp(species_nm = "Gomphosus_caeruleus",
#                                 abund_list = clean_abund_list,
#                                 dist_df = dist_df,
#                                 fish_speed = 0.5,
#                                 os = "windows",
#                                 nb_cores = 3)
# stop <- Sys.time()
# time_taken_GC <- stop - start
# saveRDS(SmaxN_GC, here::here("transformed_data", "SmaxN_GC_raw.rds"))

# 3.78 heures


## Species 3 - Chaetodon_trifasciatus

# start <- Sys.time()
# SmaxN_CT <- automat.maxN.spbysp(species_nm = "Chaetodon_trifasciatus",
#                                 abund_list = clean_abund_list,
#                                 dist_df = dist_df,
#                                 fish_speed = 0.5,
#                                 os = "windows",
#                                 nb_cores = 3)
# stop <- Sys.time()
# time_taken_CT <- stop - start
# saveRDS(SmaxN_CT, here::here("transformed_data", "SmaxN_CT_raw.rds"))

# 5 min


## Species 4 - Parupeneus_macronemus: "Parupeneus_macronemus"

# start <- Sys.time()
# SmaxN_PM <- automat.maxN.spbysp(species_nm = "Parupeneus_macronemus",
#                                 abund_list = clean_abund_list,
#                                 dist_df = dist_df,
#                                 fish_speed = 0.5,
#                                 os = "windows",
#                                 nb_cores = 3)
# stop <- Sys.time()
# time_taken_PM <- stop - start
# saveRDS(SmaxN_PM, here::here("transformed_data", "SmaxN_PM_raw.rds"))

# 19 min


## Species 5 - Thalassoma hardwicke:

start <- Sys.time()
SmaxN_TH <- automat.maxN.spbysp(species_nm = "Thalassoma_hardwicke",
                                abund_list = clean_abund_list,
                                dist_df = dist_df,
                                fish_speed = 0.5,
                                os = "windows",
                                nb_cores = 3)
stop <- Sys.time()
time_taken_TH <- stop - start
saveRDS(SmaxN_TH, here::here("transformed_data", "SmaxN_TH_raw.rds"))


## Species 6 - Parapercis hexophtalma:

start <- Sys.time()
SmaxN_PH <- automat.maxN.spbysp(species_nm = "Parapercis_hexophtalma",
                                abund_list = clean_abund_list,
                                dist_df = dist_df,
                                fish_speed = 0.5,
                                os = "windows",
                                nb_cores = 3)
stop <- Sys.time()
time_taken_PH <- stop - start
saveRDS(SmaxN_PH, here::here("transformed_data", "SmaxN_PH_raw.rds"))

## 2 - Plot ####


maxN_speed1 <- maxN_all_df_speed1
maxN_speed2 <- maxN_all_df_speed2
color_poses <- c("#66c2a5", "#fc8d62", "#8da0cb")
alpha <- 0.8
size <- 2.5
shape <- 21



fish_speed <- speed.plot2(maxN_speed1 = maxN_all_df_speed1, maxN_speed2 = maxN_all_df_speed2,
           color_poses = color_poses,
           alpha = alpha,
           size = size,
           shape = shape)

