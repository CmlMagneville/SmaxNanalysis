###############################################################################
##
## Script to execute the whole project
##
## make.R
##
## 08/12/2021 - 06/2022
##
## Camille Magneville
##
###############################################################################

# Clean the environnement:
rm(list = ls(all.names = TRUE), envir = .GlobalEnv)

# Install dependencies:
devtools::install_deps()


# Install the maxN package from Github:
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("CmlMagneville/SmaxN", build_vignettes = TRUE, force = TRUE)


# Load the functions so make them available for use:
source(here::here("R", "1_Manipulate_table_function.R"))
source(here::here("R", "2_Plot_maxN_functions.R"))
source(here::here("R", "3_Compute_combinaisons_functions.R"))
source(here::here("R", "4_Duration_per_nbcam_functions.R"))
source(here::here("R", "5_maxN_values_studied_species_functions.R"))
source(here::here("R", "6_SmaxN_across_camnb_functions.R"))
source(here::here("R", "7_SmaxN_time_functions.R"))
source(here::here("R", "8_SmaxN_fish_speed_function.R"))
source(here::here("R", "9_Stats_functions.R"))

## Load the analysis scripts:


# 1 - Exploratory analysis: Compute the different metrics and plot for C. trifasciatus:
# source(here::here("analysis", "1_exploratory_analysis.R"))

# 2 - Plot the duration of presence of species in front on different cameras simulteaneously:
source(here::here("analysis", "2_Plot_duration_nb_cam_analysis.R"))

# 3 - Plot the deltas between Smaxn & maxN and between SmaxN & SmaxN_row:
source(here::here("analysis", "3_maxN_values_delta_analysis.R"))

# 4 - Plot the evolution of SmaxN according to increasing nb of cameras:
# ... Note: take so long (8h) to be compute maxN for combinaisons so ...
# ... associated lines are commented: uncomment if want to run again
source(here::here("analysis", "4_SmaxN_increasing_nbcam_analysis.R"))

# 5 - Plot the evolution of SmaxN and maxN to increasing amount of time:
source(here::here("analysis", "5_SmaxN_increasing_timespan_analysis.R"))

# 6 - Plot the effect of fish speed on SmaxN:
source(here::here("analysis", "6_Fish_speed_analysis.R"))

# 7 - Do statistical analysis
source(here::here("analysis", "7_Stats_analysis.R"))








