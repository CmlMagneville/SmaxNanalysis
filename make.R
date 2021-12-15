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
source(here::here("R", "2_Plot_functions.R"))
source(here::here("R", "3_Compute_combinaisons_functions.R"))



# Run the script for the exercice dplyr:
source(here::here("analysis", "1_exploratory_analysis.R"))


