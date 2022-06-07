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

# PREPARE DATA
# restrict the abund_list to the studied species:
clean_abund_list <- abund_list[which(names(abund_list) %in% species_set)]

# create a dataframe which will contain maxN values for all sp:
maxN_all <- as.data.frame(matrix(ncol = 5, nrow = 1))
colnames(maxN_all) <- c("species_nm", "pose_nb", "maxN", "SmaxN", "SmaxN_timestep")


## 2 - Compute SmaxN values for each species:


## Species 1: Gomphosus_caeruleus
SmaxN_GC <- automat.maxN.spbysp(species_nm = "Gomphosus_caeruleus",
                                abund_list = clean_abund_list,
                                dist_df = dist_df,
                                fish_speed = 0.5,
                                os = "windows",
                                nb_cores = 3)




## Species 2: Ac_Cten_Dark:

# Pose 1 voir si ça marche:
abund_df <- paral_list[[1]]$abund_df
dist_df <- paral_list[[1]]$dist_df
speed <- paral_list[[1]]$speed

start <- Sys.time()
Ac_Cten_P1 <- SmaxN::SmaxN.computation(abund_df, speed, dist_df)
stop <- Sys.time()
time_taken_Ac_Cten_P1 <- stop - start

# marche en 4h


# Pose 2 voir si ça marche:
abund_df <- paral_list[[2]]$abund_df
dist_df <- paral_list[[2]]$dist_df
speed <- paral_list[[2]]$speed

start <- Sys.time()
Ac_Cten_P2 <- SmaxN::SmaxN.computation(abund_df, speed, dist_df)
stop <- Sys.time()
time_taken_Ac_Cten_P2 <- stop - start

# marche en: infini reste a la ligne 1 meme avec ajout test chemin
# essai avec message pour voir si va aux cam 2 et 3


# Pose 3 voir si ça marche:
abund_df <- paral_list[[3]]$abund_df
dist_df <- paral_list[[3]]$dist_df
speed <- paral_list[[3]]$speed

start <- Sys.time()
Ac_Cten_P3 <- SmaxN::SmaxN.computation(abund_df, speed, dist_df)
stop <- Sys.time()
time_taken_Ac_Cten_P3 <- stop - start


# si ça marche essai Pose 2 et Pose 3
# si ca marche on lance le automatize sur les 3 poses en corrigeant le code du SmaxN sur le package
# ... directement en modifiant les fonctions intermediaires (seul ça a change dans le code du pkge)
# ... on change le code des analyses SmaxN en local sur le PC avec ce que j'ai fait sur le serveur
# ... pour ces modifs cf les codes sur le bureau du pc


# remove rows with NA:
maxN_all <- maxN_all[which(! is.na(maxN_all$species_nm)), ]

# save:
saveRDS(maxN_all_df, here::here("transformed_data", "maxN_all.rds"))




## 2 - Plot

plot_deltas <- deltas.plot(maxN_all_df, colors = c("#66c2a5", "#fc8d62",
                                                   "#8da0cb"))
