###########################################################################
##
## Script to prepare data and plot the number of seconds each 17 species
## spend in front of different number of cameras simulteaneously
##
## 2_Plot_duration_nb_cam__analysis.R
##
## 04/03/2022
##
## Camille Magneville
##
###########################################################################


## 1 - Read abundance data #### (same as 1_exploratory_analysis.R)


# First, we need to call the abundance dataframes and collect them in a vector.
# I will create one vector per SD period, thus 3 vectors:


# Call LD df:

pres_abs_08_A1 <- readRDS(here::here("data", "pres_abs_08_A1.rds"))
pres_abs_08_A2 <- readRDS(here::here("data", "pres_abs_08_A2.rds"))
pres_abs_08_B1 <- readRDS(here::here("data", "pres_abs_08_B1.rds"))
pres_abs_08_B2 <- readRDS(here::here("data", "pres_abs_08_B2.rds"))
pres_abs_08_C1 <- readRDS(here::here("data", "pres_abs_08_C1.rds"))
pres_abs_08_C2 <- readRDS(here::here("data", "pres_abs_08_C2.rds"))


# Call SD df - Pose 1:

pres_abs_08_camD_Pose_1 <- readRDS(here::here("data", "pres_abs_08_camD_08_Pose_1.rds"))
pres_abs_08_camE_Pose_1 <- readRDS(here::here("data", "pres_abs_08_camE_08_Pose_1.rds"))
pres_abs_08_camF_Pose_1 <- readRDS(here::here("data", "pres_abs_08_camF_08_Pose_1.rds"))
pres_abs_08_camG_Pose_1 <- readRDS(here::here("data", "pres_abs_08_camG_08_Pose_1.rds"))
pres_abs_08_camH_Pose_1 <- readRDS(here::here("data", "pres_abs_08_camH_08_Pose_1.rds"))
pres_abs_08_camI_Pose_1 <- readRDS(here::here("data", "pres_abs_08_camI_08_Pose_1.rds"))

# Create the vector for Pose 1 but don't forget to remove the last two columns of each df...
# ... because it's time and video length:

list_abund_df_Pose_1 <- list(A1 = pres_abs_08_A1[, !(colnames(pres_abs_08_A1) %in% c("vid_length","time"))],
                             A2 = pres_abs_08_A2[, !(colnames(pres_abs_08_A2) %in% c("vid_length","time"))],
                             B1 = pres_abs_08_B1[, !(colnames(pres_abs_08_B1) %in% c("vid_length","time"))],
                             B2 = pres_abs_08_B2[, !(colnames(pres_abs_08_B2) %in% c("vid_length","time"))],
                             C1 = pres_abs_08_C1[, !(colnames(pres_abs_08_C1) %in% c("vid_length","time"))],
                             C2 = pres_abs_08_C2[, !(colnames(pres_abs_08_C2) %in% c("vid_length","time"))],
                             D = pres_abs_08_camD_Pose_1[, !(colnames(pres_abs_08_camD_Pose_1) %in% c("vid_length","time"))],
                             E = pres_abs_08_camE_Pose_1[, !(colnames(pres_abs_08_camE_Pose_1) %in% c("vid_length","time"))],
                             F = pres_abs_08_camF_Pose_1[, !(colnames(pres_abs_08_camF_Pose_1) %in% c("vid_length","time"))],
                             G = pres_abs_08_camG_Pose_1[, !(colnames(pres_abs_08_camG_Pose_1) %in% c("vid_length","time"))],
                             H = pres_abs_08_camH_Pose_1[, !(colnames(pres_abs_08_camH_Pose_1) %in% c("vid_length","time"))],
                             I = pres_abs_08_camI_Pose_1[, !(colnames(pres_abs_08_camI_Pose_1) %in% c("vid_length","time"))])


# Call SD df - Pose 2:

pres_abs_08_camD_Pose_2 <- readRDS(here::here("data", "pres_abs_08_camD_08_Pose_2.rds"))
pres_abs_08_camE_Pose_2 <- readRDS(here::here("data", "pres_abs_08_camE_08_Pose_2.rds"))
pres_abs_08_camF_Pose_2 <- readRDS(here::here("data", "pres_abs_08_camF_08_Pose_2.rds"))
pres_abs_08_camG_Pose_2 <- readRDS(here::here("data", "pres_abs_08_camG_08_Pose_2.rds"))
pres_abs_08_camH_Pose_2 <- readRDS(here::here("data", "pres_abs_08_camH_08_Pose_2.rds"))
pres_abs_08_camI_Pose_2 <- readRDS(here::here("data", "pres_abs_08_camI_08_Pose_2.rds"))

# Create the vector for Pose 2 but don't forget to remove the last two columns of each df...
# ... because it's time and video length:

list_abund_df_Pose_2 <- list(A1 = pres_abs_08_A1[, !(colnames(pres_abs_08_A1) %in% c("vid_length","time"))],
                             A2 = pres_abs_08_A2[, !(colnames(pres_abs_08_A2) %in% c("vid_length","time"))],
                             B1 = pres_abs_08_B1[, !(colnames(pres_abs_08_B1) %in% c("vid_length","time"))],
                             B2 = pres_abs_08_B2[, !(colnames(pres_abs_08_B2) %in% c("vid_length","time"))],
                             C1 = pres_abs_08_C1[, !(colnames(pres_abs_08_C1) %in% c("vid_length","time"))],
                             C2 = pres_abs_08_C2[, !(colnames(pres_abs_08_C2) %in% c("vid_length","time"))],
                             D = pres_abs_08_camD_Pose_2[, !(colnames(pres_abs_08_camD_Pose_2) %in% c("vid_length","time"))],
                             E = pres_abs_08_camE_Pose_2[, !(colnames(pres_abs_08_camE_Pose_2) %in% c("vid_length","time"))],
                             F = pres_abs_08_camF_Pose_2[, !(colnames(pres_abs_08_camF_Pose_2) %in% c("vid_length","time"))],
                             G = pres_abs_08_camG_Pose_2[, !(colnames(pres_abs_08_camG_Pose_2) %in% c("vid_length","time"))],
                             H = pres_abs_08_camH_Pose_2[, !(colnames(pres_abs_08_camH_Pose_2) %in% c("vid_length","time"))],
                             I = pres_abs_08_camI_Pose_2[, !(colnames(pres_abs_08_camI_Pose_2) %in% c("vid_length","time"))])



# Call SD df - Pose 3:

pres_abs_08_camD_Pose_3 <- readRDS(here::here("data", "pres_abs_08_camD_08_Pose_3.rds"))
pres_abs_08_camE_Pose_3 <- readRDS(here::here("data", "pres_abs_08_camE_08_Pose_3.rds"))
pres_abs_08_camF_Pose_3 <- readRDS(here::here("data", "pres_abs_08_camF_08_Pose_3.rds"))
pres_abs_08_camG_Pose_3 <- readRDS(here::here("data", "pres_abs_08_camG_08_Pose_3.rds"))
pres_abs_08_camH_Pose_3 <- readRDS(here::here("data", "pres_abs_08_camH_08_Pose_3.rds"))
pres_abs_08_camI_Pose_3 <- readRDS(here::here("data", "pres_abs_08_camI_08_Pose_3.rds"))

# Create the vector for Pose 3 but don't forget to remove the last two columns of each df...
# ... because it's time and video length:

list_abund_df_Pose_3 <- list(A1 = pres_abs_08_A1[, !(colnames(pres_abs_08_A1) %in% c("vid_length","time"))],
                             A2 = pres_abs_08_A2[, !(colnames(pres_abs_08_A2) %in% c("vid_length","time"))],
                             B1 = pres_abs_08_B1[, !(colnames(pres_abs_08_B1) %in% c("vid_length","time"))],
                             B2 = pres_abs_08_B2[, !(colnames(pres_abs_08_B2) %in% c("vid_length","time"))],
                             C1 = pres_abs_08_C1[, !(colnames(pres_abs_08_C1) %in% c("vid_length","time"))],
                             C2 = pres_abs_08_C2[, !(colnames(pres_abs_08_C2) %in% c("vid_length","time"))],
                             D = pres_abs_08_camD_Pose_3[, !(colnames(pres_abs_08_camD_Pose_3) %in% c("vid_length","time"))],
                             E = pres_abs_08_camE_Pose_3[, !(colnames(pres_abs_08_camE_Pose_3) %in% c("vid_length","time"))],
                             F = pres_abs_08_camF_Pose_3[, !(colnames(pres_abs_08_camF_Pose_3) %in% c("vid_length","time"))],
                             G = pres_abs_08_camG_Pose_3[, !(colnames(pres_abs_08_camG_Pose_3) %in% c("vid_length","time"))],
                             H = pres_abs_08_camH_Pose_3[, !(colnames(pres_abs_08_camH_Pose_3) %in% c("vid_length","time"))],
                             I = pres_abs_08_camI_Pose_3[, !(colnames(pres_abs_08_camI_Pose_3) %in% c("vid_length","time"))])


## 2 - Read distances data #### (same as 1_exploratory_analysis.R)


# Then we have to compute the distance dataframe that gathers distances between the cameras:

dist_df <- read.csv(here::here("data", "dist_df.csv"))
rownames(dist_df) <- dist_df[, 1]
dist_df <- dist_df[, -1]



## 3 - Compute abundances df for all 17 species


# create a list which contains presence absence data for the 3 poses:
abund_allposes_list <- list(list_abund_df_Pose_1, list_abund_df_Pose_2, list_abund_df_Pose_3)

# get the names of the annotated species:
sp_nm <- c(colnames(pres_abs_08_A1[, !(colnames(pres_abs_08_A1) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_A2[, !(colnames(pres_abs_08_A2) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_B1[, !(colnames(pres_abs_08_B1) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_B2[, !(colnames(pres_abs_08_B2) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_C1[, !(colnames(pres_abs_08_C1) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_C2[, !(colnames(pres_abs_08_C2) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camD_Pose_3[, !(colnames(pres_abs_08_camD_Pose_3) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camE_Pose_3[, !(colnames(pres_abs_08_camE_Pose_3) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camF_Pose_3[, !(colnames(pres_abs_08_camF_Pose_3) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camG_Pose_3[, !(colnames(pres_abs_08_camG_Pose_3) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camH_Pose_3[, !(colnames(pres_abs_08_camH_Pose_3) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camI_Pose_3[, !(colnames(pres_abs_08_camI_Pose_3) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camD_Pose_1[, !(colnames(pres_abs_08_camD_Pose_1) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camE_Pose_1[, !(colnames(pres_abs_08_camE_Pose_1) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camF_Pose_1[, !(colnames(pres_abs_08_camF_Pose_1) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camG_Pose_1[, !(colnames(pres_abs_08_camG_Pose_1) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camH_Pose_1[, !(colnames(pres_abs_08_camH_Pose_1) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camI_Pose_1[, !(colnames(pres_abs_08_camI_Pose_1) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camD_Pose_2[, !(colnames(pres_abs_08_camD_Pose_2) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camE_Pose_2[, !(colnames(pres_abs_08_camE_Pose_2) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camF_Pose_2[, !(colnames(pres_abs_08_camF_Pose_2) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camG_Pose_2[, !(colnames(pres_abs_08_camG_Pose_2) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camH_Pose_2[, !(colnames(pres_abs_08_camH_Pose_2) %in% c("vid_length","time"))]),
           colnames(pres_abs_08_camI_Pose_2[, !(colnames(pres_abs_08_camI_Pose_2) %in% c("vid_length","time"))]))

species_set <- unique(sp_nm)


# compute all abund df:
all_abund_list <- automat.abund.df(abund_allposes_list = abund_allposes_list,
                                         species_set = species_set)


saveRDS(all_abund_list, here::here("transformed_data", "all_abund_list.rds"))



# 4 - Compute a dataframe showing the number of seconds spend in front of ... ####
# ... different camera number simulteaneoulsy


duration_cam_sp <- compute.all.duration.nbcam(abund_list)


# 5 - Plot the graph showing presence duration in front of one or several ... ####
# ... simulteaneously

plot.duration.nbcam(duration_cam_sp_df = duration_cam_sp)


