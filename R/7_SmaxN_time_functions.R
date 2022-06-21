###########################################################################
##
## Script to prepare data and plot the evolution of SmaxN across an increasing
## timespan (10 min -> 1h) for the 12 cameras altogether
##
## 7_SmaxN_time_functions.R
##
## 07/03/2022
##
## Camille Magneville
##
###########################################################################


#' Create a list containing for each species, the abundance dataframes for all
#' tiemspans from 10min to 1h
#'
#' This function computes a list which contains for each species a list of
#' abundance dataframes with more or less rows (ie time) given the
#' timespan it has been build for
#'
#' @param spans_set a vector containing the timespans to use (numeric values
#' refering to the number of seconds in the timespan. For ex: 10 minutes
#' timespan = 10*60sec = 600 seconds)
#'
#' @param abund_cam_allposes_list a list gathering the abundance of each species in a
#' given dataframes with all poses fusioned and 12 cameras. Retrieved from the
#' create.abundlist.allcam.poses() function.
#'
#' @return a list containing for each species, dataframes corresponding to the
#' different timespans
#'
#' @export
#'


create.abund.list.timespan <- function(spans_set,
                                       abund_allcam_list) {



  # create three lists that will contain as many abundance df ...
  # ... as there are combinations of cameras. One list for one pose:
  final_list_pose1 <- list()
  final_list_pose2 <- list()
  final_list_pose3 <- list()


  # loop on the poses:
  for (k in (1:length(abund_allcam_list))) {

    # retrieve the studied abundance df (for the studied species and pose):
    data <- abund_allcam_list[[k]]

    # loop on the timespans:
    for (t in (1:length(spans_set))) {


      # get the first n rows of the abundance data:
      n <- spans_set[t]
      data_span <- data[c(1:n), ]


      # store the new df with only interested rows in the pose list:
      # if pose 1:
      if (k == 1) {
        final_list_pose1 <- rlist::list.append(final_list_pose1, data_span)
        names(final_list_pose1)[length(final_list_pose1)] <- n
      }

      # if pose 2:
      if (k == 2) {
        final_list_pose2 <- rlist::list.append(final_list_pose2, data_span)
        names(final_list_pose2)[length(final_list_pose2)] <- n
      }

      # if pose 3:
      if (k == 3) {
        final_list_pose3 <- rlist::list.append(final_list_pose3, data_span)
        names(final_list_pose3)[length(final_list_pose3)] <- n
      }

    } # end loop on timepans

  } # end loop on poses

  final_list <- list(final_list_pose1, final_list_pose2, final_list_pose3)
  names(final_list) <- c("Pose1", "Pose2", "Pose3")

  # return the final abund list:
  return(final_list)

}



#' Create the plot showing SmaxN values for an increasing amount of time
#'
#' This function computes plots showing SmaxN and maxN values across an
#' increasing amount of time for 12 cameras for each species (one plot = one sp)
#'
#' @param maxN_timespans a dataframe containing all maxN values, species
#' names, pose_nb, amount of time from the compute.maxN.combcam(analysis_type ="timespan")
#' function.
#'
#' @param colors a vector containing the different colors for SmaxN and maxN
#'
#' @return a ggplot2 object showing the plots of SmaxN and maxN for each
#' species and this plot is saved as pdf in the output folder
#'
#' @export
#'

timespans.plot <- function(maxN_timespans, colors, alpha, shape_pose, size) {

  # remove the "_" in the pose_nb and timespans columns:
  maxN_timespans$time_span <- gsub("_", "", as.character(maxN_timespans$time_span))
  maxN_timespans$Pose_nb <- gsub("_", "", as.character(maxN_timespans$Pose_nb))

  # pose_nb as factor:
  maxN_timespans$Pose_nb <- as.factor(maxN_timespans$Pose_nb)

  # timespan as numeric:
  maxN_timespans$time_span <- as.numeric(maxN_timespans$time_span)

  # remove NA rows:
  maxN_timespans <- maxN_timespans[which(! is.na(maxN_timespans$species_nm)), ]

  # make in long format:
  long_maxN_timespans <- reshape2::melt(maxN_timespans[, - (ncol(maxN_timespans) - 1)],
                                      id.vars = c("species_nm", "time_span", "Pose_nb"),
                                      variable.name = 'metric', value.name = 'values')

  # create a list of new labels for species:
  sp_labs <- c("C. auriga", "C. trifasciatus", "G. caeruleus",
               "O. longirostris", "P. hexophtalma",
               "P. macronemus", "T. hardwicke")
  names(sp_labs) <- c("Chaetodon_auriga", "Chaetodon_trifasciatus", "Gomphosus_caeruleus",
                      "Oxymonacanthus_longirostris", "Parapercis_hexophtalma",
                      "Parupeneus_macronemus", "Thalassoma_hardwicke")



  # plot:
  plot_timespan <- ggplot2::ggplot(data = long_maxN_timespans) +

     # ggplot2::geom_point(ggplot2::aes(x = time_span, y = values, colour = Pose_nb,
     #                                  shape = metric),
     #                                  fill = NA, show.legend = FALSE) +


    ggplot2::geom_smooth(ggplot2::aes(x = time_span, y = values, colour = metric,
                                       fill = metric),
                          method = "loess", show.legend = FALSE) +


    ggplot2::geom_jitter(ggplot2::aes(x = time_span, y = values, colour = metric,
                                      fill =  metric, alpha =  metric, shape = Pose_nb,
                                      size = metric),
                         width = 80,
                         height = 0) +


    ggplot2::scale_fill_manual(values = colors,
                               name = "Metric") +

    ggplot2::scale_colour_manual(values = colors,
                                 name = "Metric") +

    ggplot2::scale_alpha_manual(values = alpha,
                                labels = NULL) +

    ggplot2::scale_shape_manual(values = shape_pose,
                                name = "Pose number") +

    ggplot2::scale_size_manual(values = size,
                               name = "Metric") +

    ggplot2::scale_x_continuous(breaks = c(600, 1200, 1800, 2400, 3000, 3600)) +

    ggplot2::scale_y_continuous(breaks = c(1:10)) +

    ggplot2::facet_wrap(~ species_nm, ncol = 3,
                        labeller = ggplot2::labeller(species_nm = sp_labs)) +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey80"),
                   panel.grid.major = ggplot2::element_line(colour = "grey80")) +

    ggplot2::guides(alpha = "none", size = "none") +

    ggplot2::xlab("Recording time (s)") +

    ggplot2::ylab("SmaxN vs maxN")





  # save in outputs:
  # save the plot in the outputs folder:
  ggplot2::ggsave(filename = here::here("outputs/4_maxN_timespans_vers3.pdf"),
                  plot = plot_timespan,
                  device = "pdf",
                  scale = 1,
                  height = 6000,
                  width = 9000,
                  units = "px",
                  dpi = 600)

  # return the plot:
  return(plot_timespan)


}
