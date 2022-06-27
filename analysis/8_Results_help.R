###########################################################################
##
## Script of numbers to add in the results
##
## 8_Results_help.R
##
## 22/06/2022
##
## Camille Magneville
##
###########################################################################




## Help for Figure 1 ####

# 0 - Load data:

# SmaxN values:
SmaxN_all <- readRDS(here::here("transformed_data", "SmaxN_df_all_sp.rds"))

# general abundance list:
abund_list <- readRDS(here::here("transformed_data", "all_abund_list.rds"))


# 1 - Compute info:


# RATIO FOR EACH SPECIES SUMMARY

# compute mean/var/sd values of ratios for each sp SmaxN/maxN:
maxN_all %>%
  dplyr::group_by(species_nm) %>%
  dplyr::summarise(avg = mean(delta_SmaxN_maxN))

maxN_all %>%
  dplyr::group_by(species_nm) %>%
  dplyr::summarise(avg = var(delta_SmaxN_maxN))

maxN_all %>%
  dplyr::group_by(species_nm) %>%
  dplyr::summarise(avg = sd(delta_SmaxN_maxN))


# compute mean/var/sd values of ratios for each sp SmaxN/SmaxN_timestep:
maxN_all %>%
  dplyr::group_by(species_nm) %>%
  dplyr::summarise(avg = mean(delta_SmaxN_SmaxNtimestep))

maxN_all %>%
  dplyr::group_by(species_nm) %>%
  dplyr::summarise(avg = var(delta_SmaxN_SmaxNtimestep))

maxN_all %>%
  dplyr::group_by(species_nm) %>%
  dplyr::summarise(avg = sd(delta_SmaxN_SmaxNtimestep))



####


# MEAN ABUND PER SECOND

# AcCten:
# mean nb of individual per timestep per cam for each pose:
a <- mean(apply(abund_list$Ac_Cten_dark[[1]], 1, mean))
b <- mean(apply(abund_list$Ac_Cten_dark[[2]], 1, mean))
c <- mean(apply(abund_list$Ac_Cten_dark[[3]], 1, mean))
# mean on the 3 poses of the nb of individual per timestep per cam per hour:
(a+b+c)/3

# GC:
# mean nb of individual per timestep per cam for each pose:
a <- mean(apply(abund_list$Gomphosus_caeruleus[[1]], 1, mean))
b <- mean(apply(abund_list$Gomphosus_caeruleus[[2]], 1, mean))
c <- mean(apply(abund_list$Gomphosus_caeruleus[[3]], 1, mean))
# mean nb of individual per timestep per cam per hour:
(a+b+c)/3

# TH:
# mean nb of individual per timestep per cam for each pose:
a <- mean(apply(abund_list$Thalassoma_hardwicke[[1]], 1, mean))
b <- mean(apply(abund_list$Thalassoma_hardwicke[[2]], 1, mean))
c <- mean(apply(abund_list$Thalassoma_hardwicke[[3]], 1, mean))
# mean on the 3 poses of the nb of individual per timestep per cam per hour:
(a+b+c)/3

# PH:
# mean nb of individual per timestep per cam for each pose:
a <- mean(apply(abund_list$Parapercis_hexophtalma[[1]], 1, mean))
b <- mean(apply(abund_list$Parapercis_hexophtalma[[2]], 1, mean))
c <- mean(apply(abund_list$Parapercis_hexophtalma[[3]], 1, mean))
# mean on the 3 poses of the nb of individual per timestep per cam per hour:
(a+b+c)/3

# CT:
# mean nb of individual per timestep per cam for each pose:
a <- mean(apply(abund_list$Chaetodon_trifasciatus[[1]], 1, mean))
b <- mean(apply(abund_list$Chaetodon_trifasciatus[[2]], 1, mean))
c <- mean(apply(abund_list$Chaetodon_trifasciatus[[3]], 1, mean))
# mean on the 3 poses of the nb of individual per timestep per cam per hour:
(a+b+c)/3

# PM:
# mean nb of individual per timestep per cam for each pose:
a <- mean(apply(abund_list$Parupeneus_macronemus[[1]], 1, mean))
b <- mean(apply(abund_list$Parupeneus_macronemus[[2]], 1, mean))
c <- mean(apply(abund_list$Parupeneus_macronemus[[3]], 1, mean))
# mean on the 3 poses of the nb of individual per timestep per cam:
(a+b+c)/3


## AC CTEN

# number of annotation per camera
apply(try, 2, sum)

# percentage of annotation per camera:
apply(try, 2, sum)/ sum(apply(try, 2, sum))

# 4 cameras are doing between 10 and 13% of the annotations. Others: 3, 9, 7, 8, 5, 6, 5, 3



# 2 - Help for Figure 2 ####


## 1 - Prepare data :

# Load data:
speed1_df <- readRDS(here::here("transformed_data", "SmaxN_df_all_sp.rds"))
speed2_df <- readRDS(here::here("transformed_data", "SmaxN_df_all_sp_speed2.rds"))

# Compute mean and var and sd for ratio speed1/speed 2:

# but first compute a dataframe with all information:
speed1_df_final <- speed1_df[, c(1, 2, 4)]
speed2_df_final <- speed2_df[, c(1, 2, 4)]

# rename columns of SmaxN
colnames(speed1_df_final)[ncol(speed1_df_final)] <- "SmaxN_speed1"
colnames(speed2_df_final)[ncol(speed2_df_final)] <- "SmaxN_speed2"
# rename columns of maxN


# bind the two dfs:
final_speed_df <- dplyr::left_join(speed1_df_final,
                                   speed2_df_final)
# compute ratio:
final_speed_df$ratio <- final_speed_df$SmaxN_speed1 / final_speed_df$SmaxN_speed2


## 2 -  Compute interesting figures:


# prop of combination sp*poses which equals 0:
(nrow(final_speed_df[which(final_speed_df$ratio == 1), ]) / nrow(final_speed_df)) * 100


# compute mean, sd, var for each species:
final_speed_gped_df <- dplyr::group_by(final_speed_df, species_nm)
summary_speed <- dplyr::summarise(final_speed_gped_df,
                                  mean.ratio = mean(ratio),
                                  var.ratio = var(ratio),
                                  sd.ratio = sd(ratio))
