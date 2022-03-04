###########################################################################
##
## Script to explore maxN data: load Capucine presence-absence dfs, compute
## the different maxN values and plot them
##
## 1_exploratory analysis.R
##
## 11/2021
##
## Camille Magneville
##
###########################################################################



## 1 - Read abundance data ####


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


## 2 - Read distances data ####


# Then we have to compute the distance dataframe that gathers distances between the cameras:

dist_df <- read.csv(here::here("data", "dist_df.csv"))
rownames(dist_df) <- dist_df[, 1]
dist_df <- dist_df[, -1]



## 3 - Species 1 - Chateodon trifasciatus ####


# We will create a unique dataframe for Chateodon trifasciatus for each SD Pose:

# Pose 1:
CT_Pose1 <- gather.abund.df(list_abund_df = list_abund_df_Pose_1, species_nm = "Chaetodon_trifasciatus",
                            time_start = "07:30:00", time_stop = "08:30:00")
CT_P1_abund_df <- CT_Pose1$abund_df

# Pose 2:
CT_Pose2 <- gather.abund.df(list_abund_df = list_abund_df_Pose_2, species_nm = "Chaetodon_trifasciatus",
                            time_start = "11:30:00", time_stop = "12:30:00")
CT_P2_abund_df <- CT_Pose2$abund_df

# Pose 3:
CT_Pose3 <- gather.abund.df(list_abund_df = list_abund_df_Pose_3, species_nm = "Chaetodon_trifasciatus",
                            time_start = "15:30:00", time_stop = "16:30:00")
CT_P3_abund_df <- CT_Pose3$abund_df


# Then we can compute the different abundance metrics, **using 2m/s as the maximal speed** of *Chaetodon trifasciatus*:

# Pose 1:
CT_P1 <- SmaxN::compute.max.abund(dist_df = dist_df, fish_speed = 2, abund_df   = CT_P1_abund_df)

# Pose 2: remove camD from dist_df because CT is not seen during Pose 2 on camD
CT_P2 <- SmaxN::compute.max.abund(dist_df = dist_df[-6, -6], fish_speed = 2, abund_df   = CT_P2_abund_df)

# Pose 3:
CT_P3 <- SmaxN::compute.max.abund(dist_df = dist_df, fish_speed = 2, abund_df   = CT_P3_abund_df)

# Then we can prepare data to do the plot:

maxN_plot_df <- prepare.maxN.plot(list_maxN  = c(CT_P1, CT_P2, CT_P3),
                                  indices_nm = c("maxN_cam", "maxN",
                                                 "SmaxN", "SmaxN_row"))

# Then we do the plot and save it:

CT_plot <- plot.maxN(maxN_plot_df = maxN_plot_df,
                    color_points = "grey", alpha_points = 0.8,
                    color_boxplots = "aquamarine", alpha_boxplots = 0.6)

CT_plot

ggplot2::ggsave(here::here("outputs", "CT_plot.png"))



