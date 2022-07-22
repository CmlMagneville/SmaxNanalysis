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


# correlation between the SmaxN metrics with 2 speeds:
cor.test(x = final_speed_df$SmaxN_speed1, y = final_speed_df$SmaxN_speed2, method = 'spearman')


# compute mean, sd, var for each species:
final_speed_gped_df <- dplyr::group_by(final_speed_df, species_nm)
summary_speed <- dplyr::summarise(final_speed_gped_df,
                                  mean.ratio = mean(ratio),
                                  var.ratio = var(ratio),
                                  sd.ratio = sd(ratio))



# 3 - Help for Figure 3 ####


final_combcam_df <- readRDS(here::here("transformed_data", "final_combcam.rds"))


## a - compute the mean delta per species across poses between cam 1 and cam 9  - SmaxN ####

# only keep those cam:
final_combcam_df <- dplyr::filter(final_combcam_df, cam_nb %in% c(1, 9))

# mean on 9 cam (3 values as 1 time per pose) - mean on 1 cam (9 values per pose as 9 cam) for each pose ...
# ... so compare values from the same pose
# and then mean across poses:

# AcCten dark:
CS_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("AcCten_dark"))

P1 <- (CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_1"), "SmaxN"] -
  mean(CS_data[which(CS_data$cam_nb == 1 & CS_data$Pose == "Pose_1"), "SmaxN"])) /
  (CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_1"), "SmaxN"])

P2 <- (CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_2"), "SmaxN"] -
  mean(CS_data[which(CS_data$cam_nb == 1 & CS_data$Pose == "Pose_2"), "SmaxN"]))/
  (CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_2"), "SmaxN"])

P3 <- (CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_3"), "SmaxN"] -
  mean(CS_data[which(CS_data$cam_nb == 1 & CS_data$Pose == "Pose_3"), "SmaxN"]))/
  (CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_3"), "SmaxN"])


mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Chaetodon trifasciatus:
CT_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("Chaetodon_trifasciatus"))

P1 <- (CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_1"), "SmaxN"] -
  mean(CT_data[which(CT_data$cam_nb == 1 & CT_data$Pose == "Pose_1"), "SmaxN"])) /
  CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_1"), "SmaxN"]

P2 <- (CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_2"), "SmaxN"] -
  mean(CT_data[which(CT_data$cam_nb == 1 & CT_data$Pose == "Pose_2"), "SmaxN"]))/
  CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_2"), "SmaxN"]

P3 <- (CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_3"), "SmaxN"] -
  mean(CT_data[which(CT_data$cam_nb == 1 & CT_data$Pose == "Pose_3"), "SmaxN"]))/
  CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_3"), "SmaxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Gomphosus caeruleus:
GC_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("Gomphosus_caeruleus"))

P1 <- (GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_1"), "SmaxN"] -
  mean(GC_data[which(GC_data$cam_nb == 1 & GC_data$Pose == "Pose_1"), "SmaxN"]))/
  GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_1"), "SmaxN"]

P2 <- (GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_2"), "SmaxN"] -
  mean(GC_data[which(GC_data$cam_nb == 1 & GC_data$Pose == "Pose_2"), "SmaxN"]))/
  GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_2"), "SmaxN"]

P3 <- (GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_3"), "SmaxN"] -
  mean(GC_data[which(GC_data$cam_nb == 1 & GC_data$Pose == "Pose_3"), "SmaxN"]))/
  GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_3"), "SmaxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Parapercis hexophtalma:
PH_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("Parapercis_hexophtalma"))

P1 <- (PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_1"), "SmaxN"] -
  mean(PH_data[which(PH_data$cam_nb == 1 & PH_data$Pose == "Pose_1"), "SmaxN"]))/
  PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_1"), "SmaxN"]

P2 <- (PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_2"), "SmaxN"] -
  mean(PH_data[which(PH_data$cam_nb == 1 & PH_data$Pose == "Pose_2"), "SmaxN"]))/
  PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_2"), "SmaxN"]

P3 <- (PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_3"), "SmaxN"] -
  mean(PH_data[which(PH_data$cam_nb == 1 & PH_data$Pose == "Pose_3"), "SmaxN"]))/
  PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_3"), "SmaxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Parupeneus macronemus:
PM_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("Parupeneus_macronemus"))

P1 <- (PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_1"), "SmaxN"] -
  mean(PM_data[which(PM_data$cam_nb == 1 & PM_data$Pose == "Pose_1"), "SmaxN"]))/
  PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_1"), "SmaxN"]

P2 <- (PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_2"), "SmaxN"] -
  mean(PM_data[which(PM_data$cam_nb == 1 & PM_data$Pose == "Pose_2"), "SmaxN"]))/
  PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_2"), "SmaxN"]

P3 <- (PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_3"), "SmaxN"] -
  mean(PM_data[which(PM_data$cam_nb == 1 & PM_data$Pose == "Pose_3"), "SmaxN"]))/
  PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_3"), "SmaxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Thalassoma hardwicke:
TH_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("Thalassoma_hardwicke"))

P1 <- (TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_1"), "SmaxN"] -
  mean(TH_data[which(TH_data$cam_nb == 1 & TH_data$Pose == "Pose_1"), "SmaxN"]))/
  TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_1"), "SmaxN"]

P2 <- (TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_2"), "SmaxN"] -
  mean(TH_data[which(TH_data$cam_nb == 1 & TH_data$Pose == "Pose_2"), "SmaxN"]))/
  TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_2"), "SmaxN"]

P3 <- (TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_3"), "SmaxN"] -
  mean(TH_data[which(TH_data$cam_nb == 1 & TH_data$Pose == "Pose_3"), "SmaxN"]))/
  TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_3"), "SmaxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


## b - compute the mean delta per species across poses between cam 1 and cam 9  - maxN ####

# only keep those cam:
final_combcam_19_df <- dplyr::filter(final_combcam_df, cam_nb %in% c(1, 9))

# mean on 9 cam (3 values as 1 time per pose) - mean on 1 cam (9 values per pose as 9 cam) for each pose ...
# ... so compare values from the same pose
# and then mean across poses:

# AcCten dark:
CS_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("AcCten_dark"))

P1 <- (CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_1"), "maxN"] -
  mean(CS_data[which(CS_data$cam_nb == 1 & CS_data$Pose == "Pose_1"), "maxN"])) /
  CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_1"), "maxN"]

P2 <- (CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_2"), "maxN"] -
  mean(CS_data[which(CS_data$cam_nb == 1 & CS_data$Pose == "Pose_2"), "maxN"])) /
  CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_2"), "maxN"]

P3 <- (CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_3"), "maxN"] -
  mean(CS_data[which(CS_data$cam_nb == 1 & CS_data$Pose == "Pose_3"), "maxN"]))/
  CS_data[which(CS_data$cam_nb == 9 & CS_data$Pose == "Pose_3"), "maxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Chaetodon trifasciatus:
CT_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("Chaetodon_trifasciatus"))

P1 <- (CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_1"), "maxN"] -
  mean(CT_data[which(CT_data$cam_nb == 1 & CT_data$Pose == "Pose_1"), "maxN"])) /
  CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_1"), "maxN"]

P2 <- (CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_2"), "maxN"] -
  mean(CT_data[which(CT_data$cam_nb == 1 & CT_data$Pose == "Pose_2"), "maxN"])) /
  CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_2"), "maxN"]

P3 <- (CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_3"), "maxN"] -
  mean(CT_data[which(CT_data$cam_nb == 1 & CT_data$Pose == "Pose_3"), "maxN"])) /
  CT_data[which(CT_data$cam_nb == 9 & CT_data$Pose == "Pose_3"), "maxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Gomphosus caeruleus:
GC_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("Gomphosus_caeruleus"))

P1 <- (GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_1"), "maxN"] -
  mean(GC_data[which(GC_data$cam_nb == 1 & GC_data$Pose == "Pose_1"), "maxN"])) /
  GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_1"), "maxN"]

P2 <- (GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_2"), "maxN"] -
  mean(GC_data[which(GC_data$cam_nb == 1 & GC_data$Pose == "Pose_2"), "maxN"])) /
  GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_2"), "maxN"]

P3 <- (GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_3"), "maxN"] -
  mean(GC_data[which(GC_data$cam_nb == 1 & GC_data$Pose == "Pose_3"), "maxN"]))/
  GC_data[which(GC_data$cam_nb == 9 & GC_data$Pose == "Pose_3"), "maxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Parapercis hexophtalma:
PH_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("Parapercis_hexophtalma"))

P1 <- (PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_1"), "maxN"] -
  mean(PH_data[which(PH_data$cam_nb == 1 & PH_data$Pose == "Pose_1"), "maxN"]))/
  PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_1"), "maxN"]

P2 <- (PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_2"), "maxN"] -
  mean(PH_data[which(PH_data$cam_nb == 1 & PH_data$Pose == "Pose_2"), "maxN"]))/
  PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_2"), "maxN"]

P3 <- (PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_3"), "maxN"] -
  mean(PH_data[which(PH_data$cam_nb == 1 & PH_data$Pose == "Pose_3"), "maxN"])) /
  PH_data[which(PH_data$cam_nb == 9 & PH_data$Pose == "Pose_3"), "maxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Parupeneus macronemus:
PM_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("Parupeneus_macronemus"))

P1 <- (PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_1"), "maxN"] -
  mean(PM_data[which(PM_data$cam_nb == 1 & PM_data$Pose == "Pose_1"), "maxN"]))/
  PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_1"), "maxN"]

P2 <- (PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_2"), "maxN"] -
  mean(PM_data[which(PM_data$cam_nb == 1 & PM_data$Pose == "Pose_2"), "maxN"]))/
  PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_2"), "maxN"]

P3 <- (PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_3"), "maxN"] -
  mean(PM_data[which(PM_data$cam_nb == 1 & PM_data$Pose == "Pose_3"), "maxN"]))/
  PM_data[which(PM_data$cam_nb == 9 & PM_data$Pose == "Pose_3"), "maxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Thalassoma hardwicke:
TH_data <- dplyr::filter(final_combcam_19_df, species_nm %in% c("Thalassoma_hardwicke"))

P1 <- (TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_1"), "maxN"] -
  mean(TH_data[which(TH_data$cam_nb == 1 & TH_data$Pose == "Pose_1"), "maxN"]))/
  TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_1"), "maxN"]

P2 <- (TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_2"), "maxN"] -
  mean(TH_data[which(TH_data$cam_nb == 1 & TH_data$Pose == "Pose_2"), "maxN"]))/
  TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_2"), "maxN"]

P3 <- (TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_3"), "maxN"] -
  mean(TH_data[which(TH_data$cam_nb == 1 & TH_data$Pose == "Pose_3"), "maxN"]))/
  TH_data[which(TH_data$cam_nb == 9 & TH_data$Pose == "Pose_3"), "maxN"]

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


## c - compute the mSmaxN-maxN info ####


mean((final_combcam_df[which(final_combcam_df$cam_nb == 5), "SmaxN"] -
   final_combcam_df[which(final_combcam_df$cam_nb == 5), "maxN"])/
  final_combcam_df[which(final_combcam_df$cam_nb == 5), "SmaxN"])

mean((final_combcam_df[which(final_combcam_df$cam_nb == 9), "SmaxN"] -
        final_combcam_df[which(final_combcam_df$cam_nb == 9), "maxN"])/
       final_combcam_df[which(final_combcam_df$cam_nb == 9), "SmaxN"])



## d - GLMM SmaxN ####

# levels:
final_combcam_df$species_nm <- as.factor(final_combcam_df$species_nm)
final_combcam_df$cam_nb <- as.factor(final_combcam_df$cam_nb)
final_combcam_df$Pose <- as.factor(final_combcam_df$Pose)
final_combcam_df$SmaxN <- as.numeric(final_combcam_df$SmaxN)
final_combcam_df$maxN <- as.numeric(final_combcam_df$maxN)


# does SmaxN data fits poisson distrib?
poisson <- MASS::fitdistr(final_combcam_df$SmaxN, "Poisson")
car::qqp(final_combcam_df$SmaxN, "pois", lambda = poisson$estimate)

# does SmaxN data fits nbinom distrib?
nbinom <- MASS::fitdistr(final_combcam_df$SmaxN, "Negative Binomial")
car::qqp(final_combcam_df$SmaxN, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])


# but the fact is that to use the Poisson distribution: mean ~ var which is not the case here:
mean(final_combcam_df$SmaxN)
var(final_combcam_df$SmaxN)
# so use negbinom which is loosens the restrictive assumption that var ~ mean.

# try model with only slope variation:
try <- glmmTMB::glmmTMB(SmaxN ~ cam_nb + (1 | species_nm) + (1 | Pose), family = "nbinom2", data = final_combcam_df,
                        control = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS")))
summary(try)
glmmTMB:::Anova.glmmTMB(try)

# try model with slode and intercept variation:
try2 <- glmmTMB::glmmTMB(SmaxN ~ cam_nb + (1 + species_nm) + (1 + Pose), family = "nbinom2", data = final_combcam_df,
                         control = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS")))

# which one is the best? try2 so work with it
anova(try, try2)

summary(try2)

glmmTMB:::Anova.glmmTMB(try2)

# check the model:
performance::check_overdispersion(try2)
performance::check_outliers(try2)
performance::check_model(try2)


## e - GLMM maxN: ####

# levels:
final_combcam_df$species_nm <- as.factor(final_combcam_df$species_nm)
final_combcam_df$cam_nb <- as.factor(final_combcam_df$cam_nb)
final_combcam_df$Pose <- as.factor(final_combcam_df$Pose)
final_combcam_df$SmaxN <- as.numeric(final_combcam_df$SmaxN)
final_combcam_df$maxN <- as.numeric(final_combcam_df$maxN)


# does SmaxN data fits poisson distrib?
poisson <- MASS::fitdistr(final_combcam_df$maxN, "Poisson")
car::qqp(final_combcam_df$maxN, "pois", lambda = poisson$estimate)

# does SmaxN data fits nbinom distrib?
nbinom <- MASS::fitdistr(final_combcam_df$maxN, "Negative Binomial")
car::qqp(final_combcam_df$maxN, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

# thereafter I use nbinom2 (explanation before for SmaxN)

# try model with only slope variation:
try <- glmmTMB::glmmTMB(maxN ~ cam_nb + (1 | species_nm) + (1 | Pose), family = "nbinom2", data = final_combcam_df,
                        control = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS")))
summary(try)
glmmTMB:::Anova.glmmTMB(try)

# try model with slode and intercept variation:
try2 <- glmmTMB::glmmTMB(maxN ~ cam_nb + (1 + species_nm) + (1 + Pose), family = "nbinom2", data = final_combcam_df,
                         control = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS")))

# which one is the best? try2 so work with it
anova(try, try2)

summary(try2)

glmmTMB:::Anova.glmmTMB(try2)

# check the model:
performance::check_overdispersion(try2)
performance::check_outliers(try2)
performance::check_model(try2)


## f - GLMM (SmaxN - maxN) ####

# levels:
final_combcam_df$species_nm <- as.factor(final_combcam_df$species_nm)
final_combcam_df$cam_nb <- as.factor(final_combcam_df$cam_nb)
final_combcam_df$Pose <- as.factor(final_combcam_df$Pose)
final_combcam_df$SmaxN <- as.numeric(final_combcam_df$SmaxN)
final_combcam_df$maxN <- as.numeric(final_combcam_df$maxN)


# does SmaxN data fits poisson distrib?
poisson <- MASS::fitdistr((final_combcam_df$SmaxN - final_combcam_df$maxN), "Poisson")
car::qqp(final_combcam_df$SmaxN - final_combcam_df$maxN, "pois", lambda = poisson$estimate)

# does SmaxN data fits nbinom distrib?
nbinom <- MASS::fitdistr(final_combcam_df$SmaxN - final_combcam_df$maxN, "Negative Binomial")
car::qqp(final_combcam_df$SmaxN - final_combcam_df$maxN, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

# thereafter I use negative binomial distribution (explanation before)

# try model with only slope variation:
try <- glmmTMB::glmmTMB((SmaxN - maxN) ~ cam_nb + (1 | species_nm) + (1 | Pose), family = "nbinom2", data = final_combcam_df,
                        control = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS")))
summary(try)
glmmTMB:::Anova.glmmTMB(try)

# try model with slode and intercept variation:
try2 <- glmmTMB::glmmTMB((SmaxN - maxN) ~ cam_nb + (1 + species_nm) + (1 + Pose), family = "nbinom2", data = final_combcam_df,
                         control = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS")))

# which one is the best? try2 so work with it
anova(try, try2)

summary(try2)

glmmTMB:::Anova.glmmTMB(try2)

# check the model:
performance::check_outliers(try2)
performance::check_model(try2)



#



# 4 - Help for Figure 4 ####

# load data:
final_timespan_df <- readRDS(here::here("transformed_data", "final_timespan.rds"))

## a - compute the mean delta per species across poses between 600s and 3600s  - SmaxN ####

# mean on 3600s cam (3 values as 1 time per pose) - mean on 600s (3 values as 1 time per pose) for each pose ...
# ... so compare values from the same pose
# and then mean across poses:

# AcCten dark:
CS_data <- dplyr::filter(final_timespan_df, species_nm %in% c("AcCten_dark"))

P1 <- (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_1"), "SmaxN"] -
         mean(CS_data[which(CS_data$time_span == 600 & CS_data$Pose == "Pose_1"), "SmaxN"])) /
  (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_1"), "SmaxN"])

P2 <- (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_2"), "SmaxN"] -
         mean(CS_data[which(CS_data$time_span == 600 & CS_data$Pose == "Pose_2"), "SmaxN"])) /
  (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_2"), "SmaxN"])

P3 <- (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_3"), "SmaxN"] -
         mean(CS_data[which(CS_data$time_span == 600 & CS_data$Pose == "Pose_3"), "SmaxN"])) /
  (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_3"), "SmaxN"])


mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Chaetodon trifasciatus:
CT_data <- dplyr::filter(final_timespan_df, species_nm %in% c("Chaetodon_trifasciatus"))

P1 <- (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_1"), "SmaxN"] -
         mean(CT_data[which(CT_data$time_span == 600 & CT_data$Pose == "Pose_1"), "SmaxN"])) /
  (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_1"), "SmaxN"])

P2 <- (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_2"), "SmaxN"] -
         mean(CT_data[which(CT_data$time_span == 600 & CT_data$Pose == "Pose_2"), "SmaxN"])) /
  (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_2"), "SmaxN"])

P3 <- (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_3"), "SmaxN"] -
         mean(CT_data[which(CT_data$time_span == 600 & CT_data$Pose == "Pose_3"), "SmaxN"])) /
  (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_3"), "SmaxN"])

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Gomphosus caeruleus:
GC_data <- dplyr::filter(final_timespan_df, species_nm %in% c("Gomphosus_caeruleus"))

P1 <- (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_1"), "SmaxN"] -
         mean(GC_data[which(GC_data$time_span == 600 & GC_data$Pose == "Pose_1"), "SmaxN"])) /
  (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_1"), "SmaxN"])

P2 <- (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_2"), "SmaxN"] -
         mean(GC_data[which(GC_data$time_span == 600 & GC_data$Pose == "Pose_2"), "SmaxN"])) /
  (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_2"), "SmaxN"])

P3 <- (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_3"), "SmaxN"] -
         mean(GC_data[which(GC_data$time_span == 600 & GC_data$Pose == "Pose_3"), "SmaxN"])) /
  (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_3"), "SmaxN"])

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Parapercis hexophtalma:
PH_data <- dplyr::filter(final_timespan_df, species_nm %in% c("Parapercis_hexophtalma"))

P1 <- (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_1"), "SmaxN"] -
         mean(PH_data[which(PH_data$time_span == 600 & PH_data$Pose == "Pose_1"), "SmaxN"])) /
  (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_1"), "SmaxN"])

P2 <- (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_2"), "SmaxN"] -
         mean(PH_data[which(PH_data$time_span == 600 & PH_data$Pose == "Pose_2"), "SmaxN"])) /
  (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_2"), "SmaxN"])

P3 <- (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_3"), "SmaxN"] -
         mean(PH_data[which(PH_data$time_span == 600 & PH_data$Pose == "Pose_3"), "SmaxN"])) /
  (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_3"), "SmaxN"])

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Parupeneus macronemus:
PM_data <- dplyr::filter(final_timespan_df, species_nm %in% c("Parupeneus_macronemus"))

P1 <- (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_1"), "SmaxN"] -
         mean(PM_data[which(PM_data$time_span == 600 & PM_data$Pose == "Pose_1"), "SmaxN"])) /
  (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_1"), "SmaxN"])

P2 <- (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_2"), "SmaxN"] -
         mean(PM_data[which(PM_data$time_span == 600 & PM_data$Pose == "Pose_2"), "SmaxN"])) /
  (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_2"), "SmaxN"])

P3 <- (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_3"), "SmaxN"] -
         mean(PM_data[which(PM_data$time_span == 600 & PM_data$Pose == "Pose_3"), "SmaxN"])) /
  (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_3"), "SmaxN"])

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Thalassoma hardwicke:
TH_data <- dplyr::filter(final_timespan_df, species_nm %in% c("Thalassoma_hardwicke"))

P1 <- (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_1"), "SmaxN"] -
         mean(TH_data[which(TH_data$time_span == 600 & TH_data$Pose == "Pose_1"), "SmaxN"])) /
  (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_1"), "SmaxN"])

P2 <- (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_2"), "SmaxN"] -
         mean(TH_data[which(TH_data$time_span == 600 & TH_data$Pose == "Pose_2"), "SmaxN"])) /
  (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_2"), "SmaxN"])

P3 <- (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_3"), "SmaxN"] -
         mean(TH_data[which(TH_data$time_span == 600 & TH_data$Pose == "Pose_3"), "SmaxN"])) /
  (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_3"), "SmaxN"])

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


## b - compute the mean delta per species across poses between 600s and cam 3600s  - maxN ####

# mean on 3600s cam (3 values as 1 time per pose) - mean on 600s (3 values as 1 time per pose) for each pose ...
# ... so compare values from the same pose
# and then mean across poses:

# AcCten dark:
CS_data <- dplyr::filter(final_timespan_df, species_nm %in% c("AcCten_dark"))

P1 <- (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_1"), "maxN"] -
         mean(CS_data[which(CS_data$time_span == 600 & CS_data$Pose == "Pose_1"), "maxN"])) /
  (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_1"), "maxN"])

P2 <- (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_2"), "maxN"] -
         mean(CS_data[which(CS_data$time_span == 600 & CS_data$Pose == "Pose_2"), "maxN"])) /
  (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_2"), "maxN"])

P3 <- (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_3"), "maxN"] -
         mean(CS_data[which(CS_data$time_span == 600 & CS_data$Pose == "Pose_3"), "maxN"])) /
  (CS_data[which(CS_data$time_span == 3600 & CS_data$Pose == "Pose_3"), "maxN"])


mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Chaetodon trifasciatus:
CT_data <- dplyr::filter(final_timespan_df, species_nm %in% c("Chaetodon_trifasciatus"))

P1 <- (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_1"), "maxN"] -
         mean(CT_data[which(CT_data$time_span == 600 & CT_data$Pose == "Pose_1"), "maxN"])) /
  (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_1"), "maxN"])

P2 <- (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_2"), "maxN"] -
         mean(CT_data[which(CT_data$time_span == 600 & CT_data$Pose == "Pose_2"), "maxN"])) /
  (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_2"), "maxN"])

P3 <- (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_3"), "maxN"] -
         mean(CT_data[which(CT_data$time_span == 600 & CT_data$Pose == "Pose_3"), "maxN"])) /
  (CT_data[which(CT_data$time_span == 3600 & CT_data$Pose == "Pose_3"), "maxN"])

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Gomphosus caeruleus:
GC_data <- dplyr::filter(final_timespan_df, species_nm %in% c("Gomphosus_caeruleus"))

P1 <- (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_1"), "maxN"] -
         mean(GC_data[which(GC_data$time_span == 600 & GC_data$Pose == "Pose_1"), "maxN"])) /
  (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_1"), "maxN"])

P2 <- (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_2"), "maxN"] -
         mean(GC_data[which(GC_data$time_span == 600 & GC_data$Pose == "Pose_2"), "maxN"])) /
  (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_2"), "maxN"])

P3 <- (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_3"), "maxN"] -
         mean(GC_data[which(GC_data$time_span == 600 & GC_data$Pose == "Pose_3"), "maxN"])) /
  (GC_data[which(GC_data$time_span == 3600 & GC_data$Pose == "Pose_3"), "maxN"])

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Parapercis hexophtalma:
PH_data <- dplyr::filter(final_timespan_df, species_nm %in% c("Parapercis_hexophtalma"))

P1 <- (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_1"), "maxN"] -
         mean(PH_data[which(PH_data$time_span == 600 & PH_data$Pose == "Pose_1"), "maxN"])) /
  (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_1"), "maxN"])

P2 <- (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_2"), "maxN"] -
         mean(PH_data[which(PH_data$time_span == 600 & PH_data$Pose == "Pose_2"), "maxN"])) /
  (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_2"), "maxN"])

P3 <- (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_3"), "maxN"] -
         mean(PH_data[which(PH_data$time_span == 600 & PH_data$Pose == "Pose_3"), "maxN"])) /
  (PH_data[which(PH_data$time_span == 3600 & PH_data$Pose == "Pose_3"), "maxN"])

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Parupeneus macronemus:
PM_data <- dplyr::filter(final_timespan_df, species_nm %in% c("Parupeneus_macronemus"))

P1 <- (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_1"), "maxN"] -
         mean(PM_data[which(PM_data$time_span == 600 & PM_data$Pose == "Pose_1"), "maxN"])) /
  (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_1"), "maxN"])

P2 <- (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_2"), "maxN"] -
         mean(PM_data[which(PM_data$time_span == 600 & PM_data$Pose == "Pose_2"), "maxN"])) /
  (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_2"), "maxN"])

P3 <- (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_3"), "maxN"] -
         mean(PM_data[which(PM_data$time_span == 600 & PM_data$Pose == "Pose_3"), "maxN"])) /
  (PM_data[which(PM_data$time_span == 3600 & PM_data$Pose == "Pose_3"), "maxN"])

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


# Thalassoma hardwicke:
TH_data <- dplyr::filter(final_timespan_df, species_nm %in% c("Thalassoma_hardwicke"))

P1 <- (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_1"), "maxN"] -
         mean(TH_data[which(TH_data$time_span == 600 & TH_data$Pose == "Pose_1"), "maxN"])) /
  (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_1"), "maxN"])

P2 <- (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_2"), "maxN"] -
         mean(TH_data[which(TH_data$time_span == 600 & TH_data$Pose == "Pose_2"), "maxN"])) /
  (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_2"), "maxN"])

P3 <- (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_3"), "maxN"] -
         mean(TH_data[which(TH_data$time_span == 600 & TH_data$Pose == "Pose_3"), "maxN"])) /
  (TH_data[which(TH_data$time_span == 3600 & TH_data$Pose == "Pose_3"), "maxN"])

mean(c(P1, P2, P3))
var(c(P1, P2, P3))
sd(c(P1, P2, P3))


## c - compute the mSmaxN-maxN info ####


mean((final_timespan_df[which(final_timespan_df$time_span == 600), "SmaxN"] -
        final_timespan_df[which(final_timespan_df$time_span == 600), "maxN"])/
       final_timespan_df[which(final_timespan_df$time_span == 600), "SmaxN"])

mean((final_timespan_df[which(final_timespan_df$time_span == 1800), "SmaxN"] -
        final_timespan_df[which(final_timespan_df$time_span == 1800), "maxN"])/
       final_timespan_df[which(final_timespan_df$time_span == 1800), "SmaxN"])

mean((final_timespan_df[which(final_timespan_df$time_span == 3600), "SmaxN"] -
        final_timespan_df[which(final_timespan_df$time_span == 3600), "maxN"])/
       final_timespan_df[which(final_timespan_df$time_span == 3600), "SmaxN"])



## d - GLMM SmaxN ####

# levels:
final_timespan_df$species_nm <- as.factor(final_timespan_df$species_nm)
final_timespan_df$time_span <- as.factor(final_timespan_df$time_span)
final_timespan_df$Pose <- as.factor(final_timespan_df$Pose)
final_timespan_df$SmaxN <- as.numeric(final_timespan_df$SmaxN)
final_timespan_df$maxN <- as.numeric(final_timespan_df$maxN)

# check distrib:
ggplot2::ggplot(final_timespan_df, ggplot2::aes(SmaxN)) +
       ggplot2::geom_bar(fill = "#1E90FF") +
       ggplot2::theme_classic() +
       ggplot2::theme(legend.position="none")
# does not look like any known distribution


# try scaling data:
final_timespan_scaled_df <- final_timespan_df
final_timespan_scaled_df <- scale(final_timespan_scaled_df[, c(3, 4, 5)])
final_timespan_scaled_df <- cbind(final_timespan_scaled_df, final_timespan_df[, c(1, 2, 6)])

# plot new distribution
ggplot2::ggplot(final_timespan_scaled_df, ggplot2::aes(SmaxN)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")
# no

# try loging data:
final_timespan_log_df <- final_timespan_df
final_timespan_log_df$maxN <- log(final_timespan_log_df$maxN + 1)
final_timespan_log_df$SmaxN <- log(final_timespan_log_df$SmaxN + 1)
final_timespan_log_df$SmaxN_timestep<- log(final_timespan_log_df$SmaxN_timestep + 1)


# plot new distribution
ggplot2::ggplot(final_timespan_log_df, ggplot2::aes(SmaxN)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")
# still no

# raw data looks like a quasi poisson with the end of the distrib:

try <- stats::glm(SmaxN ~ time_span + (1+species_nm) + (1+Pose),
                  family = "quasipoisson", data = final_timespan_df)

car::Anova(try)
summary(try)

performance::check_overdispersion(try)
performance::check_outliers(try)
performance::check_model(try)


## e - GLMM maxN: ####

# levels:
final_timespan_df$species_nm <- as.factor(final_timespan_df$species_nm)
final_timespan_df$cam_nb <- as.factor(final_timespan_df$cam_nb)
final_timespan_df$Pose <- as.factor(final_timespan_df$Pose)
final_timespan_df$SmaxN <- as.numeric(final_timespan_df$SmaxN)
final_timespan_df$maxN <- as.numeric(final_timespan_df$maxN)

# check distrib:
ggplot2::ggplot(final_timespan_df, ggplot2::aes(SmaxN)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")
# does not look like any known distribution


# raw data looks like a quasi poisson with the end of the distrib:

try <- stats::glm(maxN ~ time_span + (1+species_nm) + (1+Pose),
                  family = "quasipoisson", data = final_timespan_df)

car::Anova(try)
summary(try)

performance::check_overdispersion(try)
performance::check_outliers(try)
performance::check_model(try)




## f - GLMM (SmaxN - maxN) ####

# levels:
final_timespan_df$species_nm <- as.factor(final_timespan_df$species_nm)
final_timespan_df$cam_nb <- as.factor(final_timespan_df$cam_nb)
final_timespan_df$Pose <- as.factor(final_timespan_df$Pose)
final_timespan_df$SmaxN <- as.numeric(final_timespan_df$SmaxN)
final_timespan_df$maxN <- as.numeric(final_timespan_df$maxN)


# check distrib:
ggplot2::ggplot(final_timespan_df, ggplot2::aes(SmaxN - maxN)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")
# does not look like any known distribution


# raw data looks like a quasi poisson with the end of the distrib:

try <- stats::glm(SmaxN - maxN ~ time_span + (1+species_nm) + (1+Pose),
                  family = "quasipoisson", data = final_timespan_df)

car::Anova(try)
summary(try)

performance::check_overdispersion(try)
performance::check_outliers(try)
performance::check_model(try)




