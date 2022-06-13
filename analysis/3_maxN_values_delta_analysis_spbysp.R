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

# ICRS: keep only 6 cameras:
dist_df <- dist_df[c(2, 4, 6, 8, 10, 12), c(2, 4, 6, 8, 10, 12)]
dist_df <- dist_df[c(1, 3, 5, 7, 9, 11), c(1, 3, 5, 7, 9, 11)]

# ICRS: keep only 9 cameras (6 LD and 3 SD):
dist_df <- dist_df[c(2, 4, 6, 7, 8, 9, 10, 11, 12), c(2, 4, 6, 7, 8, 9, 10, 11, 12)]


## 1 - Prepare data ####

# get the names of species which are of interest:
species_set <- c("Gomphosus_caeruleus", "Parupeneus_macronemus",
                 "Chaetodon_auriga", "Parapercis_hexophtalma",
                 "Chaetodon_trifasciatus", "Thalassoma_hardwicke",
                 "Oxymonacanthus_longirostris", "Ac_Cten_dark")

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

# create a dataframe which will contain maxN values for all sp:
maxN_all <- as.data.frame(matrix(ncol = 5, nrow = 1))
colnames(maxN_all) <- c("species_nm", "pose_nb", "maxN", "SmaxN", "SmaxN_timestep")


## 2 - Compute SmaxN values for each species: code commented as long to run and no need to again



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
# SmaxN_AcCten <- automat.maxN.spbysp(species_nm = "Ac_Cten_dark",
#                                 abund_list = clean_abund_list,
#                                 dist_df = dist_df,
#                                 fish_speed = 0.5,
#                                 os = "windows",
#                                 nb_cores = 3)
# stop <- Sys.time()
# time_taken_AcCten <- stop - start
# saveRDS(SmaxN_AcCten, here::here("transformed_data", "SmaxN_AcCten_raw.rds"))

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

## 2 - Arrange data from parallelisation process for plot

# call data:
SmaxN_AcCten <- readRDS(here::here("transformed_data", "SmaxN_AcCten_raw.rds"))
SmaxN_CT <- readRDS(here::here("transformed_data", "SmaxN_CT_raw.rds"))
SmaxN_GC <- readRDS(here::here("transformed_data", "SmaxN_GC_raw.rds"))
SmaxN_PM <- readRDS(here::here("transformed_data", "SmaxN_PM_raw.rds"))


# gather data for all species:
all_sp_list <- list(SmaxN_AcCten, SmaxN_CT, SmaxN_GC, SmaxN_PM)
names(all_sp_list) <- c("AcCten_dark", "Chaetodon_trifasciatus",
                        "Gomphosus_caeruleus", "Parupeneus_macronemus")


# to a clean table:
SmaxN_df_all_sp <- clean.df.maxN(all_sp_list = all_sp_list)



## 3 - Plot

plot_deltas <- deltas.plot(maxN_all = SmaxN_df_all_sp,
                           colors = c("#66c2a5", "#fc8d62",
                                                   "#8da0cb"))
