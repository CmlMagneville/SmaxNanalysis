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
                                       abund_cam_allposes_list) {



  # create the abundance list which will contain for each species ...
  # ... one df per studied tiemspan:
  abund_list_final <- list()


  # for all species:
  for (i in (1:length(abund_cam_allposes_list))) {


    # create a list that will contain new abundance data for each species:
    abund_time_sp_list <- list()


    # create three df refering each to one pose and gather them in a list:
    pose <- abund_cam_allposes_list[[i]]
    pose1 <- pose[which(rownames(pose) %in% as.character(hms::as_hms(c(hms::as_hms("07:30:00"):hms::as_hms("08:30:00"))))), ]
    pose2 <- pose[which(rownames(pose) %in% as.character(hms::as_hms(c(hms::as_hms("11:30:00"):hms::as_hms("12:30:00"))))), ]
    pose3 <- pose[which(rownames(pose) %in% as.character(hms::as_hms(c(hms::as_hms("15:30:00"):hms::as_hms("16:30:00"))))), ]
    pose_abund_list <- list(pose1, pose2, pose3)

    # loop on the three poses (as the timespan can not be within two poses ...
    # ... ex 5 minutes in Pose1 and 5 minutes in Pose2):
    for (j in (1:length(pose_abund_list))) {

      # create a counter that will give a unique id nb to each abundance df ...
      # ... for each pose_timespan_rep (pose1_10min_rep1 -> 1, pose 1_10min_rep2 -> id2)
      id <- 1


      # loop on the timespans:
      for (k in (1:length(spans_set))) {


          # get the first n rows of the abundance data:
          n <- spans_set[k]
          data <- pose_abund_list[[j]]
          data_span <- data[c(1:n), ]

          # store it in the new abundance list:
          abund_time_sp_list <- rlist::list.append(abund_time_sp_list, data_span)

          # rename the element with timespan and repetition info:
          names(abund_time_sp_list)[length(abund_time_sp_list)] <- paste0(n, sep = "_",
                                                                              "pose", sep ="", j,
                                                                              sep = "_", id)

          # remove the first n rows from the abundance data:
          data <- data[-c(1:n), ]

          # update id value
          id <- id + 1

          # then, while there is enough rows in the abund df, continue to store ...
          # ... abund df for the studied timespan:
          while(nrow(data) >= n) {

            data_span <- data[c(1:n), ]
            abund_time_sp_list <- rlist::list.append(abund_time_sp_list, data_span)
            names(abund_time_sp_list)[length(abund_time_sp_list)] <- paste0(n, sep = "_",
                                                                                "pose", sep ="", j,
                                                                                sep = "_", id)
            data <- data[-c(1:n), ]
            id <- id +1

          }


      } # end loop on timepans

    } # end loop on poses

    # add the abund list of the species to the final abund list:
    abund_list_final <- rlist::list.append(abund_list_final, abund_time_sp_list)
    names(abund_list_final)[length(abund_list_final)] <- names(abund_cam_allposes_list)[i]

  } # end loop on species

  # save the final df in transformed_data folder
  saveRDS(abund_list_final, here::here("transformed_data", "abund_list_timespans.rds"))

  # return the final abund list:
  return(abund_list_final)

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

timespans.plot <- function(maxN_timespans, colors, alpha, shape, size) {

  # remove "_" from the time spans and numerise:
  # how to do it?

  # remove NA rows:
  maxN_timespans <- maxN_timespans[which(! is.na(maxN_timespans$species_nm)), ]

  # make in long format:
  long_maxN_timespans <- reshape2::melt(maxN_timespans[, - ncol(maxN_timespans)],
                                      id.vars = c("species_nm", "time_span", "pose_nb"),
                                      variable.name = 'metric', value.name = 'values')


  # plot JE ME SUIS ARRETE ICI:
  plot_combcam <- ggplot2::ggplot(data = long_maxN_combcam) +

    ggplot2::geom_point(ggplot2::aes(x = cam_nb, y = values, colour = metric,
                                     fill = metric, alpha = metric, shape = metric,
                                     size = metric)) +

    ggplot2::geom_smooth(ggplot2::aes(x = cam_nb, y = values, colour = metric,
                                      fill = metric),
                         method = "loess", show.legend = FALSE) +


    ggplot2::scale_fill_manual(values = colors,
                               name = "Metric") +

    ggplot2::scale_colour_manual(values = colors,
                                 name = "Metric") +

    ggplot2::scale_alpha_manual(values = alpha,
                                labels = NULL) +

    ggplot2::scale_shape_manual(values = shape,
                                name = "Metric") +

    ggplot2::scale_size_manual(values = size,
                               name = "Metric") +

    ggplot2::scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +

    ggplot2::facet_wrap(~ species_nm, ncol = 3) +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey"),
                   panel.grid.major = ggplot2::element_line(colour = "grey")) +

    ggplot2::guides(colour = "none", alpha = "none", size = "none") +

    ggplot2::xlab("Camera number") +

    ggplot2::ylab("SmaxN vs maxN")



  # save in outputs:
  # save the plot in the outputs folder:
  ggplot2::ggsave(filename = here::here("outputs/3_maxN_combcam.pdf"),
                  plot = plot_combcam,
                  device = "pdf",
                  scale = 1,
                  height = 5000,
                  width = 9000,
                  units = "px",
                  dpi = 600)

  # return the plot:
  return(plot_combcam)


}
