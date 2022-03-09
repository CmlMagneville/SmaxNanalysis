###############################################################################
##
## Script to execute the whole project
##
## make.R
##
## 08/12/2021
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
remotes::install_github("CmlMagneville/SmaxN", build_vignettes = TRUE)


# Load the functions so make them available for use:
source(here::here("R", "1_Manipulate_table_function.R"))
source(here::here("R", "2_Plot_maxN_functions.R"))
source(here::here("R", "3_Compute_combinaisons_functions.R"))
source(here::here("R", "4_Duration_per_nbcam_functions.R"))
source(here::here("R", "5_maxN_values_studied_species_functions.R"))
source(here::here("R", "6_SmaxN_3h_across_camnb_functions.R"))

## Load the analysis scripts:


# 1 - Exploratory analysis: Compute the different metrics and plot for C. trifasciatus:
# source(here::here("analysis", "1_exploratory_analysis.R"))

# 2 - Plot the duration of presence of species in front on different cameras simulteaneously:
source(here::here("analysis", "2_Plot_duration_nb_cam_analysis.R"))

# 3 - Plot the deltas between Smaxn & maxN and between SmaxN & SmaxN_row:
source(here::here("analysis", "3_maxN_values_delta_analysis.R"))

# 4 - Plot the evolution of SmaxN according to increasing nb of cameras:
# ... Note: take so long (8h) to be compute maxN for combinaisons!
source(here::here("analysis", "4_SmaxN_increasing_nbcam_analysis.R"))







