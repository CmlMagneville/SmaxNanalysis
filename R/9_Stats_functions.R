###########################################################################
##
## Script to do the statistical analysis
##
## 9_Stats_functions.R
##
## 30/03/2022
##
## Camille Magneville
##
###########################################################################




#' Plot SmaxN vs maxN for each pose and do a correlation
#'
#' This function computes a list of abundance dataframes for each species.
#' For each species, the dataframes contain the
#' full number of camers (ie 12)
#'
#' @param maxN_all a dataframe from the automat.maxN.setsp() function with SmaxN,
#' maxN, SmaxN_row, species names and Poses number columns
#'
#' @param colors_poses a vector containing the names of the colors to use to plot
#' the different poses
#'
#' @param comp_metric the name of the metric to compare with the SmaxN. It could
#' either be "SmaxN_row" or "maxN". ONLY for the plot, correlations tests are
#' made for SmaxN_row and maxN.
#'
#' @return a plot saved in the output folder and the resultats of the correlation
#' test
#'
#' @export
#'

cor.SmaxN.plot<- function(maxN_all, colors_poses, comp_metric) {


  # be sure the variables have the right class:
  maxN_all$maxN <- as.numeric(maxN_all$maxN)
  maxN_all$SmaxN <- as.numeric(maxN_all$SmaxN)
  maxN_all$SmaxN_row <- as.numeric(maxN_all$SmaxN_row)

  maxN_all$pose_nb <- as.factor(maxN_all$pose_nb)


  # plot maxN vs Smax for the 3 poses

  # if to be compared with maxN:
  if (comp_metric == "maxN") {

    SmaxN_plot <- ggplot2::ggplot(data = maxN_all) +

      ggplot2::geom_jitter(ggplot2::aes(x = maxN, y = SmaxN, color = pose_nb)) +

      ggplot2::geom_smooth(ggplot2::aes(x = maxN, y = SmaxN, color = pose_nb),
                           method = "lm", se = FALSE) +

      ggplot2::geom_abline(color = "grey50") +

      ggplot2::scale_colour_manual(values = colors_poses,
                                   name = "Poses",
                                   labels = c("Pose 1: 7:30-8:30",
                                              "Pose 2: 11:30-12:30",
                                              "Pose 3: 15:30-16:30")) +
      ggplot2::scale_x_continuous(limits = c(0, 12),
                                  breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +

      ggplot2::scale_y_continuous(limits = c(0, 12),
                                  breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +

      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey"),
                     panel.grid.major = ggplot2::element_line(colour = "grey"),
                     strip.text.y = ggplot2::element_text(size = 8))
  }


  # if to be compared with SmaxN_row:
  if (comp_metric == "SmaxN_row") {

    SmaxN_plot <- ggplot2::ggplot(data = maxN_all) +

      ggplot2::geom_jitter(ggplot2::aes(x = SmaxN_row, y = SmaxN, color = pose_nb)) +

      ggplot2::geom_smooth(ggplot2::aes(x = SmaxN_row, y = SmaxN, color = pose_nb),
                           method = "lm", se = FALSE) +

      ggplot2::geom_abline(color = "grey50") +

      ggplot2::scale_colour_manual(values = colors_poses,
                                   name = "Poses",
                                   labels = c("Pose 1: 7:30-8:30",
                                              "Pose 2: 11:30-12:30",
                                              "Pose 3: 15:30-16:30")) +
      ggplot2::scale_x_continuous(limits = c(0, 12),
                                  breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +

      ggplot2::scale_y_continuous(limits = c(0, 12),
                                  breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +

      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey"),
                     panel.grid.major = ggplot2::element_line(colour = "grey"),
                     strip.text.y = ggplot2::element_text(size = 8))
  }



  # and save it in the output folder:

  if (comp_metric == "maxN") {
    ggplot2::ggsave(filename = here::here("outputs/Stat_1_Correl_SmaxN_maxN.pdf"),
                    plot = SmaxN_plot,
                    device = "pdf",
                    scale = 1,
                    height = 5000,
                    width = 6000,
                    units = "px",
                    dpi = 600)
  }

  if (comp_metric == "SmaxN_row") {
    ggplot2::ggsave(filename = here::here("outputs/Stat_1_Correl_SmaxN_SmaxNrow.pdf"),
                    plot = SmaxN_plot,
                    device = "pdf",
                    scale = 1,
                    height = 5000,
                    width = 6000,
                    units = "px",
                    dpi = 600)
  }



  # test correlation:
  cor_maxN <- cor.test(x = maxN_all$maxN, y = maxN_all$SmaxN, method = 'spearman')
  cor_SmaxNrow <- cor.test(x = maxN_all$SmaxN_row, y = maxN_all$SmaxN, method = 'spearman')

  # create the returned list
  return_list <- list(SmaxN_plot, cor_maxN, cor_SmaxNrow)

  return(return_list)

}



