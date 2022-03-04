###########################################################################
##
## Scripts to compute and plot the number of seconds each species spend in front a
## different number of cameras
##
## 4_Duration_per_nbcam.R
##
## 03/03/2022
##
## Camille Magneville
##
###########################################################################


#' Create a dataframe containing number of seconds spend simulteaneously
#' in front on different camera number
#'
#' This function computes for a given species, a dataframe containing the number
#' of seconds the species is seen on different camera number simulteaneoulsy for
#' all the Poses
#'
#' @param species_nm the latin name of the studied species given with no
#' space but underscore "_" between Genera and Species names
#'
#' @param list_abund a list gathering the abundance dataframes of the different
#' Poses (dataframes with cameras = columns and time = rows)
#'
#' @return a dataframe with three columns: species name (same for all rows),
#' nb_cam (the number of cameras on which the species is seen simulteaneously)
#' and duration (the number of seconds the species is seen on a given number of
#' cameras)
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

compute.duration.nbcam <- function(species_nm, list_abund){

  # create a new df that will contain species_nm, nb_cam and duration:
  duration_cam_df <- as.data.frame(matrix(ncol = 3, nrow = 1))
  colnames(duration_cam_df) <- c("species_nm", "cam_nb", "duration")

  # loop on the different Poses:
  for (i in (1:length(list_abund))) {

    table <- list_abund[[i]]

    # change each value > 1 to 1 in table so can count nb of cam easily:
    table2 <- table %>% dplyr::mutate_if(is.numeric, ~1 * (. > 1))

    # create a new column: each row is the sum of all the column = nb of cam
    # ... for a given second:
    table3 <- tibble::add_column(table, nb_cam = rowSums(table2))

    # fill the duration_cam_df:
    for (nb_cam in (unique(table3$nb_cam))) {

      # which nb_cam are already present in the duration_cam_df?
      cam_vect <- unique(duration_cam_df$cam_nb)

      # compute the number of seconds:
      sec_nb <- nrow(table3[which(table3$nb_cam == nb_cam), ])

      # if nb_cam already in the duration_cam_df:
      if (nb_cam %in% cam_vect) {
        duration_cam_df[which(duration_cam_df$cam_nb == nb_cam),
                        "duration"] <- duration_cam_df[which(duration_cam_df$cam_nb == nb_cam), "duration"] + sec_nb
      }

      # if nb_cam not in the duration_cam_df:
      if (! nb_cam %in% cam_vect) {
        duration_cam_df <- dplyr::add_row(duration_cam_df,
                                          species_nm = species_nm,
                                          cam_nb = nb_cam,
                                          duration = sec_nb)
      }

    }

    # remove the first empty row:
    duration_cam_df <- duration_cam_df[-1, ]



  }

  # return the duration df:
  return(duration_cam_df)

}
