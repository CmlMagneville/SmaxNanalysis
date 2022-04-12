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



################################################################################



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
theoretic_count <- rpois(nrow(SmaxN_df[which(! is.na(SmaxN_df$species_nm)), ])
                         , mean(SmaxN_df[which(! is.na(SmaxN_df$species_nm)), "SmaxN"]))
tc_df <- data.frame(theoretic_count)

ggplot2::ggplot(SmaxN_df[which(! is.na(SmaxN_df$species_nm)), ], ggplot2::aes(SmaxN)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theoretic_count,fill = "#1E90FF", alpha = 0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")

# other way to test:
poisson <- MASS::fitdistr(SmaxN_df$SmaxN, "Poisson")
car::qqp(SmaxN_df$SmaxN, "pois", lambda = poisson$estimate)

nbinom <- MASS::fitdistr(SmaxN_df$SmaxN, "Negative Binomial")
car::qqp(SmaxN_df$SmaxN, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])


## 2 - a/  Compute GLMM for camera number effect (pose and species random effects) ####

SmaxN_df <- maxN_comb_cam
Y_var <- "SmaxN"
X_var <- "cam_nb"
X_var_random <- c("species_nm", "Pose_nb")
family_law <- "poisson"
check_resid <- TRUE
compute_RNakag <- TRUE

model_cam_nb <- glmm.compute(SmaxN_df = SmaxN_df,
                             Y_var = Y_var,
                             X_var = X_var,
                             X_var_random = X_var_random,
                             family_law = family_law,
                             check_resid = TRUE,
                             compute_RNakag = TRUE)


# other tests:
performance::check_model(model_cam_nb[[1]])
performance::model_performance(model_cam_nb[[1]])



## 2 - b/ Compute GLMM for metric and camera number effect (pose and sp random) ####



################################################################################



# 3 - Test significant differences of SmaxN with an increasing recording time ####


# Load data:
maxN_timespan <- readRDS(here::here("transformed_data", "final_timespans.rds"))

# Param:
SmaxN_df <- maxN_timespan
metric <- "timespan"

# plot:
kruskal.SmaxN.plot(SmaxN_df, metric)



## 3 - 0/ Which distribution to use for SmaxN? ####



# Check Poisson (randomly distributed):
theoretic_count <- rpois(nrow(SmaxN_df[which(! is.na(SmaxN_df$species_nm)), ])
                         , mean(SmaxN_df[which(! is.na(SmaxN_df$species_nm)), "SmaxN"]))
tc_df <- data.frame(theoretic_count)

ggplot2::ggplot(SmaxN_df[which(! is.na(SmaxN_df$species_nm)), ], ggplot2::aes(SmaxN)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theoretic_count,fill = "#1E90FF", alpha = 0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")

# other way to test (poisson and nbinom):
poisson <- MASS::fitdistr(SmaxN_df[which(! is.na(SmaxN_df$species_nm)), "SmaxN"], "Poisson")
car::qqp(SmaxN_df$SmaxN, "pois", lambda = poisson$estimate)

nbinom <- MASS::fitdistr(SmaxN_df[which(! is.na(SmaxN_df$species_nm)), "SmaxN"], "Negative Binomial")
car::qqp(SmaxN_df$SmaxN, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])



## 3 - a/  Compute GLMM for timespan effect (pose and species random effects) ####


SmaxN_df <- maxN_timespan
Y_var <- "SmaxN"
X_var <- "time_span"
X_var_random <- c("species_nm", "Pose_nb")
family_law <- "poisson"
check_resid <- TRUE
compute_RNakag <- TRUE

model_timespan <- glmm.compute(SmaxN_df = SmaxN_df,
                             Y_var = Y_var,
                             X_var = X_var,
                             X_var_random = X_var_random,
                             family_law = family_law,
                             check_resid = TRUE,
                             compute_RNakag = TRUE)

## 3 - 02/ Which distribution to use for abundance? ####



# Check Poisson (randomly distributed):
theoretic_count <- rpois(nrow(SmaxN_df2[which(! is.na(SmaxN_df2$species_nm)), ])
                         , mean(SmaxN_df2[which(! is.na(SmaxN_df$species_nm)), "values"]))
tc_df <- data.frame(theoretic_count)

ggplot2::ggplot(SmaxN_df2[which(! is.na(SmaxN_df2$species_nm)), ], ggplot2::aes(values)) +
  ggplot2::geom_bar(fill = "#1E90FF") +
  ggplot2::geom_bar(data = tc_df, ggplot2::aes(theoretic_count,fill = "#1E90FF", alpha = 0.5)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position="none")

# other way to test (poisson and nbinom):
poisson <- MASS::fitdistr(SmaxN_df2[which(! is.na(SmaxN_df2$species_nm)), "values"], "Poisson")
car::qqp(SmaxN_df2$values, "pois", lambda = poisson$estimate)

nbinom <- MASS::fitdistr(SmaxN_df2[which(! is.na(SmaxN_df2$species_nm)), "values"], "Negative Binomial")
car::qqp(SmaxN_df2$values, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])


## 3 - b/ Compute GLMM for metric and timespan effect (pose and sp random)


# make a metric column so we can test the effect of the metric on abundance estim:
SmaxN_df2 <- reshape2::melt(SmaxN_df,
                    id.vars = c("species_nm", "time_span", "Pose_nb"),
                    variable.name = 'metric', value.name = 'values')


Y_var <- "values"
X_var <- c("cam_nb", "metric")
X_var_random <- c("species_nm", "Pose_nb")
family_law <- "poisson"
check_resid <- TRUE
compute_RNakag <- TRUE


model_timespan_maxN <- glmm.compute(SmaxN_df = SmaxN_df2,
                               Y_var = Y_var,
                               X_var = X_var,
                               X_var_random = X_var_random,
                               family_law = family_law,
                               check_resid = TRUE,
                               compute_RNakag = TRUE)


# 4 - Test significant differences between SmaxN with different speed (for all species) ####




