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


## Load the analysis scripts:


# 1 - Exploratory analysis: Compute the different metrics and plot for C. trifasciatus:
source(here::here("analysis", "1_exploratory_analysis.R"))

# 2 - Plot the duration of presence of species in front on different cameras simulteaneously:
source(here::here("analysis", "2_Plot_duration_nb_cam_analysis.R"))









