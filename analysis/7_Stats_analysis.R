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
maxN_combcam <- readRDS(here::here("transformed_data", "final_combcam.rds"))

# Param:
SmaxN_df <- maxN_combcam
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


## 2 - 1/  Compute GLMM for camera number effect (pose and species random effects) ####

SmaxN_df <- maxN_comb_cam
Y_var <- "SmaxN"
X_var <- "cam_nb"
X_var_random <- c("species_nm", "Pose")
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



# NEW ANALYSIS: use fct and change it with that
# https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html
try <- glmmTMB::glmmTMB(SmaxN~cam_nb +(1 | species_nm) + (1 | Pose), family = "poisson", data=SmaxN_df)
summary(try)
glmmTMB:::Anova.glmmTMB(try)

try2 <- glmmTMB::glmmTMB(SmaxN~cam_nb + (1 + species_nm) + (1 + Pose), family = "poisson", data=SmaxN_df)

anova(try, try2)

summary(try2)

glmmTMB:::Anova.glmmTMB(try2)

tdat <- data.frame(predicted=predict(try2), residual = residuals(try2))
ggplot2::ggplot(tdat,ggplot2::aes(x=predicted,y=residual, colour=species_nm)) + ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept=0, lty=3)
ggplot2::ggplot(tdat,ggplot2::aes(x=predicted,y=residual, colour=Pose_nb)) + ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept=0, lty=3)


ggplot2::ggplot(tdat,ggplot2::aes(x=residual)) + ggplot2::geom_histogram(bins=20, color="black")
ggplot2::ggplot(tdat,ggplot2::aes(sample=residual)) + ggplot2::stat_qq() + ggplot2::stat_qq_line()

performance::check_overdispersion(try2)
performance::check_outliers(try2)

performance::check_model(try2)


## 2 - 2/ Which distribution for abundance metric? ####



SmaxN_df2 <- reshape2::melt(maxN_combcam,
               id.vars = c("species_nm", "cam_nb", "comb_nm", "Pose"),
               variable.name = 'metric', value.name = 'values')

# remove the SmaxN_timespan metric which does not interest us for now:
SmaxN_df2 <- SmaxN_df2[which(SmaxN_df2$metric != "SmaxN_timestep"), ]
SmaxN_df2$Pose <- as.factor(SmaxN_df2$Pose)


SmaxN_df2$values <- as.integer(SmaxN_df2$values)
SmaxN_df2$cam_nb <- as.factor(SmaxN_df2$cam_nb)
SmaxN_df2$species_nm <- as.factor(SmaxN_df2$species_nm)
SmaxN_df2$metric <- as.factor(SmaxN_df2$metric)


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


## 2 - 3/ Compute GLMM for metric and camera number effect (pose and sp random) ####



# NEW ANALYSIS:
# https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html

try <- glmmTMB::glmmTMB(values ~ metric * cam_nb +(1 | species_nm) + (1 | Pose),
                        family = "poisson", data=SmaxN_df2)
summary(try)
glmmTMB:::Anova.glmmTMB(try)

try2 <- glmmTMB::glmmTMB(values ~ metric * cam_nb +(1 + species_nm) + (1 + Pose),
                         family = "poisson", data=SmaxN_df2)

anova(try, try2)

summary(try2)

glmmTMB:::Anova.glmmTMB(try2)

tdat <- data.frame(predicted=predict(try2), residual = residuals(try2))
ggplot(tdat,aes(x=predicted,y=residual)) + geom_point() + geom_hline(yintercept=0, lty=3)
ggplot(tdat,aes(x=predicted,y=residual, colour=Pose_nb)) + geom_point() + geom_hline(yintercept=0, lty=3)


ggplot2::ggplot(tdat,ggplot2::aes(x=residual)) + ggplot2::geom_histogram(bins=20, color="black")
ggplot2::ggplot(tdat,ggplot2::aes(sample=residual)) + ggplot2::stat_qq() + ggplot2::stat_qq_line()

performance::check_overdispersion(try2)
performance::check_outliers(try2)

performance::check_model(try2)

performance::model_performance(try2) # to have the R2


tukey <- TukeyHSD(aov(try2))
tukey$cam_nb


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


# NEW ANALYSIS: use fct and change it with that
# https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html

SmaxN_df <- SmaxN_df[which(! is.na(SmaxN_df$species_nm)), ]

try <- glmmTMB::glmmTMB(SmaxN ~ time_span +(1 | species_nm) + (1 | Pose_nb), family = poisson, data=SmaxN_df)
summary(try)
glmmTMB:::Anova.glmmTMB(try)

try2 <- glmmTMB::glmmTMB(SmaxN~ time_span +(1 + species_nm) + (1 + Pose_nb), family = poisson, data=SmaxN_df)

anova(try, try2)

summary(try2)

glmmTMB:::Anova.glmmTMB(try2)

tdat <- data.frame(predicted=predict(try2), residual = residuals(try2))
ggplot(tdat,aes(x=predicted,y=residual)) + geom_point() + geom_hline(yintercept=0, lty=3)
ggplot(tdat,aes(x=predicted,y=residual, colour=Pose_nb)) + geom_point() + geom_hline(yintercept=0, lty=3)


ggplot(tdat,aes(x=residual)) + geom_histogram(bins=20, color="black")
ggplot(tdat,aes(sample=residual)) + stat_qq() + stat_qq_line()

performance::check_overdispersion(try2)
performance::check_outliers(try2)

performance::check_model(try2)


performance::model_performance(try2)



## 3 - 02/ Which distribution to use for abundance? ####

SmaxN_df2 <- reshape2::melt(SmaxN_df[, -5],
                            id.vars = c("species_nm", "time_span", "Pose_nb"),
                            variable.name = 'metric', value.name = 'values')

SmaxN_df2$values <- as.integer(SmaxN_df2$values)
SmaxN_df2$time_span <- as.factor(SmaxN_df2$time_span)
SmaxN_df2$species_nm <- as.factor(SmaxN_df2$species_nm)
SmaxN_df2$Pose_nb <- as.factor(SmaxN_df2$Pose_nb)
SmaxN_df2$metric <- as.factor(SmaxN_df2$metric)


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


## 3 - b/ Compute GLMM for metric and timespan effect (pose and sp random) ####

# NEW ANALYSIS:
# https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html

try <- glmmTMB::glmmTMB(values ~ metric * time_span +(1 | species_nm) + (1 | Pose_nb),
                        family = poisson, data=SmaxN_df2)
summary(try)
glmmTMB:::Anova.glmmTMB(try)

try2 <- glmmTMB::glmmTMB(values ~ metric * time_span +(1 + species_nm) + (1 + Pose_nb),
                         family = poisson, data=SmaxN_df2)

anova(try, try2)

summary(try2)

glmmTMB:::Anova.glmmTMB(try2)

tdat <- data.frame(predicted=predict(try2), residual = residuals(try2))
ggplot(tdat,aes(x=predicted,y=residual)) + geom_point() + geom_hline(yintercept=0, lty=3)
ggplot(tdat,aes(x=predicted,y=residual, colour=Pose_nb)) + geom_point() + geom_hline(yintercept=0, lty=3)


ggplot(tdat,aes(x=residual)) + geom_histogram(bins=20, color="black")
ggplot(tdat,aes(sample=residual)) + stat_qq() + stat_qq_line()

performance::check_overdispersion(try2)
performance::check_outliers(try2)

performance::check_model(try2)

performance::model_performance(try2) # to have the R2


# make a metric column so we can test the effect of the metric on abundance estim:



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

# Load data:
maxN_speed1 <- readRDS(here::here("transformed_data", "maxN_all_df_speed1.rds"))
maxN_speed2 <- readRDS(here::here("transformed_data", "maxN_all_df_speed2.rds"))

# gather with speed info:
maxN_speed1$speed <- rep(0.5, nrow(maxN_speed1))
maxN_speed2$speed <- rep(1, nrow(maxN_speed2))

maxN_speed <- dplyr::bind_rows(maxN_speed1, maxN_speed2)

# class:
maxN_speed$species_nm <- as.factor(maxN_speed$species_nm)
maxN_speed$pose_nb <- as.factor(maxN_speed$pose_nb)
maxN_speed$maxN <- as.integer(maxN_speed$maxN)
maxN_speed$SmaxN <- as.integer(maxN_speed$SmaxN)
maxN_speed$speed <- as.factor(maxN_speed$speed)


## 4 - 0/ Which distribution to use for SmaxN? ####

SmaxN_df <- maxN_speed

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



## 4 - a/  Compute GLMM for speed effect (pose and species random effects) ####



# NEW ANALYSIS: use fct and change it with that
# https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html

SmaxN_df <- SmaxN_df[which(! is.na(SmaxN_df$species_nm)), ]

try <- glmmTMB::glmmTMB(SmaxN ~ speed +(1 | species_nm) + (1 | pose_nb), family = poisson, data=SmaxN_df)
summary(try)
glmmTMB:::Anova.glmmTMB(try)

try2 <- glmmTMB::glmmTMB(SmaxN~ speed +(1 + species_nm) + (1 + pose_nb), family = poisson, data=SmaxN_df)

anova(try, try2)

summary(try2)

glmmTMB:::Anova.glmmTMB(try2)

tdat <- data.frame(predicted=predict(try2), residual = residuals(try2))
ggplot(tdat,aes(x=predicted,y=residual)) + geom_point() + geom_hline(yintercept=0, lty=3)
ggplot(tdat,aes(x=predicted,y=residual, colour=Pose_nb)) + geom_point() + geom_hline(yintercept=0, lty=3)


ggplot(tdat,aes(x=residual)) + geom_histogram(bins=20, color="black")
ggplot(tdat,aes(sample=residual)) + stat_qq() + stat_qq_line()

performance::check_overdispersion(try2)
performance::check_outliers(try2)

performance::check_model(try2)


performance::model_performance(try2)


## 4 - 02/ Which distribution to use for abundance? ####

SmaxN_df2 <- reshape2::melt(SmaxN_df[, -5],
                            id.vars = c("species_nm", "speed", "pose_nb"),
                            variable.name = 'metric', value.name = 'values')

SmaxN_df2$values <- as.integer(SmaxN_df2$values)
SmaxN_df2$speed <- as.factor(SmaxN_df2$speed)
SmaxN_df2$species_nm <- as.factor(SmaxN_df2$species_nm)
SmaxN_df2$pose_nb <- as.factor(SmaxN_df2$pose_nb)
SmaxN_df2$metric <- as.factor(SmaxN_df2$metric)


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


## 3 - b/ Compute GLMM for metric and timespan effect (pose and sp random) ####

# NEW ANALYSIS:
# https://mspeekenbrink.github.io/sdam-r-companion/linear-mixed-effects-models.html

try <- glmmTMB::glmmTMB(values ~ metric * speed +(1 | species_nm) + (1 | pose_nb),
                        family = poisson, data=SmaxN_df2)
summary(try)
glmmTMB:::Anova.glmmTMB(try)

try2 <- glmmTMB::glmmTMB(values ~ metric * speed +(1 + species_nm) + (1 + pose_nb),
                         family = poisson, data=SmaxN_df2)

anova(try, try2)

summary(try2)

glmmTMB:::Anova.glmmTMB(try2)

tdat <- data.frame(predicted=predict(try2), residual = residuals(try2))
ggplot(tdat,aes(x=predicted,y=residual)) + geom_point() + geom_hline(yintercept=0, lty=3)
ggplot(tdat,aes(x=predicted,y=residual, colour=Pose_nb)) + geom_point() + geom_hline(yintercept=0, lty=3)


ggplot(tdat,aes(x=residual)) + geom_histogram(bins=20, color="black")
ggplot(tdat,aes(sample=residual)) + stat_qq() + stat_qq_line()

performance::check_overdispersion(try2)
performance::check_outliers(try2)

performance::check_model(try2)

performance::model_performance(try2) # to have the R2



