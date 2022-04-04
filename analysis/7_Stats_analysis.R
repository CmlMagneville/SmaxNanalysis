###########################################################################
##
## Script of statistical analysis
##
## 7_Stats_analysis.R
##
## 30/03/2022
##
## Camille Magneville
##
###########################################################################



# 1 - Test significant differences between SmaxN and maxN (and SmaxN_row) for all species ####

# Load data
maxN_all <- readRDS(here::here("transformed_data", "maxN_all.rds"))

# param:
maxN_all <- maxN_all
color_poses <- c("#66c2a5", "#fc8d62", "#8da0cb")
shape_sp <- c(3, 10, 4, 13, 21, 23, 24)
comp_metric <- "SmaxN_row"

# call fct to do the plot and the analysis (can do it for each pose also
# ... by using maxN_all[which(pose_nb == 1), ] but here keep all poses ...
# ... to save the graph with all poses:
cor.SmaxN.plot(maxN_all = maxN_all,
               color_poses = color_poses,
               shape_sp = shape_sp,
               comp_metric = comp_metric)



# 2 - Test significant differences of SmaxN with an increasing number of cameras ####


# Load data:
maxN_comb_cam <- readRDS(here::here("transformed_data", "final_combcam.rds"))

# Param:
SmaxN_df <- maxN_comb_cam
metric <- "cam_nb"

# plot and kruskall-wallis (finally not used in the paper):
kruskal.SmaxN.plot(SmaxN_df, metric)


## 2 - 0/ Which distribution to use for SmaxN? ####


# Check Poisson (randomly distributed):
theoretic_count <-rpois(nrow(SmaxN_df), mean(SmaxN_df$SmaxN))
tc_df <- data.frame(theoretic_count)

ggplot2::ggplot(SmaxN_df, ggplot2::aes(SmaxN)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theoretic_count,fill = "#1E90FF", alpha = 0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")



## 2 - a/  Compute GLMM for camera number effect (pose and species random effects)

SmaxN_df <- maxN_comb_cam
Y_var <- "SmaxN"
X_var <- "cam_nb"
X_var_random <- c("species_nm", "Pose_nb")
family_law <- "poisson"
check_resid <- TRUE
compute_RNakag <- TRUE



## 2 - b/ Compute GLMM for metric and camera number effect (pose and sp random)






# 3 - Test significant differences of SmaxN with an increasing recording time ####


# Load data:
maxN_timespan <- readRDS(here::here("transformed_data", "maxN_timespans.rds"))

# Param:
SmaxN_df <- maxN_timespan
metric <- "timespan"

# plot:
kruskal.SmaxN.plot(SmaxN_df, metric)


## 3 - a/  Compute GLMM for timespan effect (pose and species random effects)

SmaxN_df <- maxN_comb_cam
Y_var <- "SmaxN"
X_var <- "time_span"
X_var_random <- c("species_nm", "pose_nb")
family_law <- "poisson"
check_resid <- TRUE
compute_RNakag <- TRUE



## 3 - b/ Compute GLMM for metric and timespan effect (pose and sp random)


# 4 - Test significant differences between SmaxN with different speed (for all species) ####




