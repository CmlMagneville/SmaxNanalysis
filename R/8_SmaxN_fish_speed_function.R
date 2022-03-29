###########################################################################
##
## Function to plot the effect of fish speed according to the species identity
##
## 6_SmaxN_fish_speed_function.R
##
## 29/03/2022
##
## Camille Magneville
##
###########################################################################



#' Create and save the plot of SmaxN for two different fish speeds and a set
#' of species
#'
#' This function computes and save in the outputs folder a plot showing SmaxN
#' values for two different fish speeds and a set of species
#'
#' @param maxN_speed1 a dataframe from the automat.maxN.setsp() function
#' gathering maxN values for each species of the set and for each pose computed
#' with the first speed
#'
#' @param maxN_speed2 a dataframe from the automat.maxN.setsp() function
#' gathering maxN values for each species of the set and for each pose computed
#' with the second speed
#'
#' @param color a vector containing the names or hexadecimal code of the two
#' colors used to plot the different speed
#'
#' @param alpha a vector containing the values of the two
#' transparencies used to plot the different speed
#'
#' @param size a numerical value referring to the size of the points used to
#' plot SmaxN
#'
#' @param shape three numerical value referring to the shape of the points used to
#' plot SmaxN for each pose
#'
#' @return a plot with y = SmaxN values with two different colors for the two
#' different speeds, x = species
#'
#' @export
#'


speed.plot <- function(maxN_speed1, maxN_speed2, color_sp, alpha, size, shape_poses) {


  ## bind the two dfs and only keep SmaxN values:

  # only keep SmaxN Pose and species information:
  maxN_speed1_final <- maxN_speed1[, c(1, 2, 4)]
  maxN_speed2_final <- maxN_speed2[, c(1, 2, 4)]

  # rename columns of SmaxN
  colnames(maxN_speed1_final)[ncol(maxN_speed1_final)] <- "SmaxN_speed1"
  colnames(maxN_speed2_final)[ncol(maxN_speed2_final)] <- "SmaxN_speed2"

  # bind the two dfs:
  final_speed_df <- dplyr::left_join(maxN_speed1_final,
                                     maxN_speed2_final)

  # factorise:
  final_speed_df$species_nm <- as.factor(final_speed_df$species_nm)
  final_speed_df$pose_nb <- as.factor(final_speed_df$pose_nb)


  # plot:
  speed_plot <- ggplot2::ggplot(data = final_speed_df) +

    ggplot2::geom_jitter(ggplot2::aes(x = SmaxN_speed2, y = SmaxN_speed1, colour = species_nm,
                                      fill =  species_nm, shape = pose_nb), alpha = alpha,
                                      size = size,
                                      width = 0.15,
                                      height = 0.15) +

    ggplot2::geom_abline(color = "grey50") +

    ggplot2::scale_fill_manual(values = color_sp,
                               name = NULL,
                               labels = NULL) +

    ggplot2::scale_colour_manual(values = color_sp,
                                 name = "Species",
                                 labels = c("C. auriga", "C. trifasciatus", "G. caeruleus",
                                            "O. longirostris", "P. hexophtalma",
                                            "P. macronemus", "T. hardwicke")) +

    ggplot2::scale_shape_manual(values = shape_pose,
                                 name = "Pose",
                                 labels = c("Pose 1: 7:30-8:30",
                                            "Pose 2: 11:30-12:30",
                                            "Pose 3: 15:30-16:30")) +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey"),
                   panel.grid.major = ggplot2::element_line(colour = "grey")) +

    ggplot2::xlab("SmaxN - 2m/s") +

    ggplot2::ylab("SmaxN - 1m/s") +

    ggplot2::scale_x_continuous(limits = c(1, 12),
                                breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +

    ggplot2::scale_y_continuous(limits = c(1, 12),
                                breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +

    ggplot2::guides(fill = "none", alpha = "none", size = "none")


  # save in outputs:
  # save the plot in the outputs folder:
  ggplot2::ggsave(filename = here::here("outputs/6_Fish_speed_SmaxN.pdf"),
                  plot = speed_plot,
                  device = "pdf",
                  scale = 1,
                  height = 4000,
                  width = 5000,
                  units = "px",
                  dpi = 600)

  # return the plot:
  return(speed_plot)

}

