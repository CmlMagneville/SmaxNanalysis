###########################################################################
##
## Script to prepare data and plot the amount of time needed for each species
## apparition
##
## 8_First_apparition_functions.R
##
## 16/03/2022
##
## Camille Magneville
##
###########################################################################


#' Create a dataframe containing for each species, the amount of time needed to see
#' the species with 12 cameras
#'
#' This function computes dataframe which contains for each species the amount
#' of time in seconds to see the species
#'
#' @param abund_list_allposes a list gathering the abundance dataframes of the different
#' species for all cameras and poses fusionneed
#'
#' @return a dataframe containing for each species, the amount of time needed to see
#' the species with 12 cameras
#'
#'
#' @export
#'


first.app <- function(abund_list_allposes) {


  # create a dataframe that will contain the data:
  first_app_df <- as.data.frame(matrix(ncol = 2, nrow = 1))
  colnames(first_app_df) <- c("species_nm", "first_app_time")

  # loop on the different species:
  for(i in c(1:length(abund_list_allposes))) {

    nrow <-

  }


}
