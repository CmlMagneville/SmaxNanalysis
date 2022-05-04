###########################################################################
##
## Scripts to compute and plot the number of seconds each species spend in front a
## different number of cameras
##
## 4_Duration_per_nbcam_functions.R
##
## 03/03/2022
##
## Camille Magneville
##
###########################################################################


#' Create a dataframe containing number of seconds spend simulteaneously
#' in front on different camera number for a given species
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
  for (j in (1:length(list_abund))) {

    table <- list_abund[[j]]

    # change each value > 1 to 1 in table so can count nb of cam easily:
    table2 <- table %>% dplyr::mutate_if(is.numeric, ~1 * (. >= 1))

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

  }

  # remove the first empty row:
  duration_cam_df <- duration_cam_df[-1, ]

  # return the duration df:
  return(duration_cam_df)

}




#' Create a dataframe containing number of seconds spend simulteaneously
#' in front on different camera number for ALL species
#'
#' This function computes for all species, a dataframe containing the number
#' of seconds the species is seen on different camera number simulteaneoulsy for
#' all the Poses
#'
#' @param abund_list a list gathering the abundance dataframes of the different
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


compute.all.duration.nbcam <- function(abund_list) {

  # create a df that will contain values for all species:
  duration_nbcam_all_df <- as.data.frame(matrix(ncol = 3, nrow = 1))
  colnames(duration_nbcam_all_df) <- c("species_nm", "cam_nb", "duration")

  # loop on species:
  for (i in (1:length(abund_list))) {

    # compute the duration_nbcam_df for the studied species:
    duration_sp <- compute.duration.nbcam(species_nm = names(abund_list[i]),
                           abund_list[[i]])

    # add it to the global df:
    duration_nbcam_all_df <- dplyr::bind_rows(duration_nbcam_all_df, duration_sp)

  }

  # remove first row of the global df:
  duration_nbcam_all_df <- duration_nbcam_all_df[-1 , ]

  # return the global df:
  return(duration_nbcam_all_df)

}



#' Create plot showing the number of seconds spend simulteaneously
#' in front on different camera number for the 17 species
#'
#' This function computes for all species, a plot: x = species,
#' y = duration of presence, colour = nb of cameras
#'
#' @param duration_cam_sp_df a dataframe containing species, nb_cam and
#' duration columns
#'
#' @return a plot which is saved in the output folder
#'
#' @export
#'


plot.duration.nbcam <- function(duration_cam_sp_df) {

  # convert as factor the cam_nb column:
  duration_cam_sp_df$cam_nb <- as.factor(duration_cam_sp_df$cam_nb)

  # remove rows with 0:
  duration_cam_sp_df <- duration_cam_sp_df[which(duration_cam_sp_df$cam_nb != 0), ]

  # add a total duration column so can order decreasingly the bar:
  duration_cam_sp_df$tot_presence <- rep(0, nrow(duration_cam_sp_df))

  # fill it:
  for (k in (unique(duration_cam_sp_df$species_nm))) {
    duration_cam_sp_df$tot_presence[which(duration_cam_sp_df$species_nm == k)] <-  sum(duration_cam_sp_df$duration[which(duration_cam_sp_df$species_nm == k)])
  }

  # plot:
  durat_cam_plot <- ggplot2::ggplot(data = duration_cam_sp_df,
                                   ggplot2::aes(x = reorder(species_nm, -tot_presence),
                                                y = duration)) +

    ggplot2::geom_bar(ggplot2::aes(fill = cam_nb), stat = "identity")  +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey"),
                   panel.grid.major = ggplot2::element_line(colour = "grey")) +

    ggplot2::labs("Camera number") +

    ggplot2::xlab("species name")


  # save the plot in the outputs folder:
  ggplot2::ggsave(filename = here::here("outputs/1_Duration_nb_cam_all_sp.pdf"),
                  plot = durat_cam_plot,
                  device = "pdf",
                  scale = 1,
                  height = 4000,
                  width = 5000,
                  units = "px",
                  dpi = 600)

}
