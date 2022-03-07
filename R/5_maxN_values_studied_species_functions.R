###########################################################################
##
## Scripts to automatise the computation of maxN values for a set of species
## and build an informative df that will be use to plot deltas
##
## 5_maxN_values_studied_species_functions.R
##
## 03/03/2022
##
## Camille Magneville
##
###########################################################################


#' Create a dataframe containing maxN values for the studied species and
#' the three poses
#'
#' This function computes a dataframes containing maxN,SmaxN, maxN_row,
#' delta1 = SmaxN - maxN, delta2 = SmaxN - SmaxN_row for a given set of
#' species and the three poses
#'
#' @param species_set a vector containing the latin name of the studied species
#' given with no space but underscore "_" between Genera and Species names
#'
#' @param abund_list a list gathering the abundance dataframes of the different
#' Poses (dataframes with cameras = columns and time = rows)
#'
#' @param dist_df  a numerical dataframe containing the distance between each
#'  pair of camera. There are as many rows as there are cameras and there are
#'  as many columns as there are cameras, thus the dataframe is symmetrical
#'  and the diagonal is filled with 0. \strong{Rows names and columns names
#'  must be cameras names}. \strong{BE CAREFUL that the cameras are
#' the same and in the same order in the dist_df and in the abund_df!}
#'
#' @param fish_speed a numerical value refering to the maximal speed of the
#'  studied species. \strong{Speed must be given in meters per second}. If the
#'  computation of maxN values should not take into account fish speed (that is
#'  to say if the camera pooling is done at the second level),
#'  \code{fish_speed = NULL}
#'
#' @return a dataframe with seven columns: maxN,SmaxN, maxN_row,
#' delta1 = SmaxN - maxN, delta2 = SmaxN - SmaxN_row, Species_nm, Poses
#'
#' @importFrom magrittr %>%
#'
#' @export
#'


automat.maxN.setsp <- function(species_set, abund_list, dist_df, fish_speed) {

  # restrict the abund_list to the studied species:
  clean_abund_list <- abund_list[which(names(abund_list) %in% species_set)]

  # create a dataframe which will cintain maxN values for all sp:
  maxN_all <- as.data.frame(matrix(ncol = 5, nrow = 1))
  colnames(maxN_all) <- c("species_nm", "pose_nb", "maxN", "SmaxN", "SmaxN_row")


  # loop on species:
  for (i in (1:length(clean_abund_list))) {

    print(paste0(names(clean_abund_list[i]), sep = " ", "starts"))


    # create a dataframe which will contain maxN data for the studied sp:
    maxN_sp <- as.data.frame(matrix(ncol = 5, nrow = 1))
    colnames(maxN_sp) <- c("species_nm", "pose_nb", "maxN", "SmaxN", "SmaxN_row")

    # loop on the different poses:
    for (j in (1:length(clean_abund_list[[i]]))) {

      print(paste0("Pose", sep = " ", j, p = " ", "starts"))


      # compute maxN values (be cereful that dist_df has the right nb columns):
      maxN_data <- SmaxN::compute.max.abund(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[j]])),
                                                              which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[j]]))],
                                            fish_speed = fish_speed,
                                            abund_df = clean_abund_list[[i]][[j]])

      # put maxN values in the maxN_sp df:
      new_row <- tibble::tibble(species_nm = names(clean_abund_list[i]),
                               pose_nb = j,
                               maxN = maxN_data$maxN,
                               SmaxN = maxN_data$SmaxN,
                               SmaxN_row = maxN_data$SmaxN_row)
      maxN_sp <- dplyr::add_row(maxN_sp, new_row)

      print(paste0("Pose", sep = " ", j, p = " ", "ends"))


    }

    maxN_all <- dplyr::bind_rows(maxN_all, maxN_sp)

    print(paste0(names(clean_abund_list[i]), sep = " ", "ends"))

  }

  # remove rows with NA:
  maxN_all <- maxN_all[which(! is.na(maxN_all$species_nm)), ]

  # save:
  saveRDS(maxN_all, here::here("transformed_data", "maxN_all.rds"))

  # return global maxN:
  return(maxN_all)

}


#' Create the plot showing delta between maxN values for a set of species
#'
#' This function computes a plot showing SmaxN, maxN, SmaxN_row (values on y)
#' for a given set of species (x) and the three poses (several colors)
#' and save the plot in outputs folder
#'
#' @param maxN_all a dataframe containing all maxN values, deltas, species
#' names and poses (obtained from automat.maxN.setsp() function)
#'
#' @param colors a vector containiing the different colors for the three poses
#'
#' @return a ggplot2 plot showing delta1 = SmaxN - maxN and
#' delta2 = SmaxN - SmaxN_row (values on y) for a set of species (x) and the
#' different poses and different facets for deltas
#'
#' @export
#'


deltas.plot <- function(maxN_all, colors) {

  # factorise poses:
  maxN_all$pose_nb <- as.factor(maxN_all$pose_nb)

  # factorise species:
  maxN_all$species_nm <- as.factor(maxN_all$species_nm)

  # compute deltas:
  maxN_all$delta_SmaxN_maxN <- maxN_all$SmaxN - maxN_all$maxN
  maxN_all$delta_SmaxN_SmaxNrow <- maxN_all$SmaxN - maxN_all$SmaxN_row


  # plot for delta 1:
  plot_delta1 <- ggplot2::ggplot(data = maxN_all,
                                 ggplot2::aes(x = species_nm, y = delta_SmaxN_maxN)) +

    ggplot2::geom_jitter(ggplot2::aes(fill = pose_nb, colour = pose_nb),
                        shape = 21, size = 2.5,
                        width = 0.15,
                        height = 0) +

    ggplot2::scale_fill_manual(values = colors,
                               name = "Pose number") +

    ggplot2::scale_colour_manual(values = colors,
                               name = "Pose number") +

    ggplot2::scale_alpha_manual(labels = NULL) +

  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                 panel.background = ggplot2::element_rect(fill = "white",
                                                          colour = "grey"),
                 panel.grid.major = ggplot2::element_line(colour = "grey")) +

  ggplot2::xlab("Species name") +

  ggplot2::ylab("Delta 1 = SmaxN - maxN")


  # plot for delta 2:
  plot_delta2 <- ggplot2::ggplot(data = maxN_all,
                                 ggplot2::aes(x = species_nm, y = delta_SmaxN_SmaxNrow)) +

    ggplot2::geom_jitter(ggplot2::aes(fill = pose_nb, colour = pose_nb),
                         shape = 21, size = 2.5,
                         width = 0.15,
                         height = 0) +

    ggplot2::scale_fill_manual(values = colors,
                               name = "Pose number") +

    ggplot2::scale_colour_manual(values = colors,
                                 name = "Pose number") +

    ggplot2::scale_alpha_manual(labels = NULL) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey"),
                   panel.grid.major = ggplot2::element_line(colour = "grey")) +

    ggplot2::xlab("Species name") +

    ggplot2::ylab("Delta 2 = SmaxN - SmaxN_row")

  # gather the two plots using patchwork:
  patchwork_plot <- plot_delta1 + plot_delta2 +
    patchwork::plot_layout(byrow = TRUE, ncol = 2, nrow = 1,
                           guides = "collect")


  # save in outputs:
  # save the plot in the outputs folder:
  ggplot2::ggsave(filename = here::here("outputs/2_Deltas_maxN.pdf"),
                  plot = patchwork_plot,
                  device = "pdf",
                  scale = 1,
                  height = 4000,
                  width = 7500,
                  units = "px",
                  dpi = 600)

  # return the plot:
  return(patchwork_plot)


}

