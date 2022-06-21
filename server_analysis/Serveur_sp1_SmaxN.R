###########################################################################
##
## Script to launch on the server to compute SmaxN and other metrics for
## ... a given species : Ac_Cten_dark
##
## Serveur_sp1_SmaxN.R
##
## 06/06/2022
##
## Camille Magneville
##
###########################################################################



# Source the scripts we need so we have functions to parallelise ...
# ... SmaxN computation on the three poses:

source(here::here("R", "5_maxN_values_studied_species_functions.R"))


# Load the SmaxN package from Github:
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("CmlMagneville/SmaxN", build_vignettes = TRUE, force = TRUE)


# Load needed data:
abund_list <- readRDS(here::here("transformed_data", "all_abund_list.rds"))
dist_df <- read.csv(here::here("data", "dist_df.csv"), row.names = 1)

# Compute for the species 1:
species_nm <- "Ac_Cten_dark"
SmaxN_AcCten_dark <- automat.maxN.spbysp(species_nm,
                    abund_list,
                    dist_df,
                    fish_speed = 0.5,
                    os = "linux",
                    nb_cores = 3)
saveRDS(SmaxN_AcCten_dark$maxN_sp, "SmaxN_AcCten_dark_df.rds")
saveRDS(SmaxN_AcCten_dark$time.taken, "SmaxN_AcCten_dark_timetaken.rds")

