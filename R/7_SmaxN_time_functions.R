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
    # remove first NA row:
    data <- data[-1, ]

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
#' @param compare a caracter string refering to the metric which is compared
#' to SmaxN: either "maxN" or "SmaxN_timestep"
#'
#' @return a ggplot2 object showing the plots of SmaxN and maxN for each
#' species and this plot is saved as pdf in the output folder
#'
#' @export
#'

timespans.plot <- function(maxN_timespans, colors, alpha, shape_pose, size, compare) {

  # remove the "_" in the pose_nb and timespans columns:
  maxN_timespans$time_span <- gsub("_", "", as.character(maxN_timespans$time_span))
  maxN_timespans$Pose <- gsub("_", "", as.character(maxN_timespans$Pose))

  # pose_nb as factor:
  maxN_timespans$Pose <- as.factor(maxN_timespans$Pose)

  # timespan as numeric:
  maxN_timespans$time_span <- as.numeric(maxN_timespans$time_span)

  # make in long format:
  long_maxN_timespans <- reshape2::melt(maxN_timespans,
                                      id.vars = c("species_nm", "time_span", "Pose"),
                                      variable.name = 'metric', value.name = 'values')

  # rename AcCten_dark in Ctenochaetus_striatus
  ac_cten <- long_maxN_timespans[which(long_maxN_timespans$species_nm == "AcCten_dark"), ]
  ac_cten$species_nm <- rep("Ctenochaetus_striatus", nrow(ac_cten))
  ac_cten$species_nm <- as.character(ac_cten$species_nm)
  long_maxN_timespans <- long_maxN_timespans[which(! long_maxN_timespans$species_nm == "AcCten_dark"), ]

  long_maxN_timespans <- rbind(long_maxN_timespans, ac_cten)
  long_maxN_timespans$species_nm <- as.factor(long_maxN_timespans$species_nm)
  long_maxN_timespans$species_nm <- forcats::fct_relevel(long_maxN_timespans$species_nm, c("Chaetodon_trifasciatus",
                                                                                       "Ctenochaetus_striatus",
                                                                                       "Gomphosus_caeruleus",
                                                                                       "Parupeneus_macronemus",
                                                                                       "Parapercis_hexophtalma",
                                                                                       "Thalassoma_hardwicke"))

  # remove unwanted rows according to the wanted graph = SmaxN vs maxN or SmaxN vs SmaxN_timestep:
  if (compare == "maxN") {
    long_maxN_timespans <- long_maxN_timespans[which(! long_maxN_timespans$metric == "SmaxN_timestep"), ]
    long_maxN_timespans$metric <- as.factor(long_maxN_timespans$metric)
  }

  if (compare == "SmaxN_timestep") {
    long_maxN_timespans <- long_maxN_timespans[which(! long_maxN_timespans$metric == "maxN"), ]
    long_maxN_timespans$metric <- as.factor(long_maxN_timespans$metric)
  }


  # create a list of new labels for species:
  # create a list of new labels for species:
  sp_labs <- c("C. trifasciatus",
               "C. striatus",
               "G. caeruleus",
               "P. macronemus",
               "P. hexophtalma",
               "T. hardwicke")
  names(sp_labs) <- c("Chaetodon_trifasciatus",
                      "Ctenochaetus_striatus",
                      "Gomphosus_caeruleus",
                      "Parupeneus_macronemus",
                      "Parapercis_hexophtalma",
                      "Thalassoma_hardwicke")

  # create a list of new labels for poses:
  pose_labs <- c("Pose 1: 7:30-8:30", "Pose 2: 11:30-12:30", "Pose3: 15:30-16:30")
  names(pose_labs) <- c("Pose1", "Pose2", "Pose3")

  # add a column which is a combination af Pose and metric (to be able to plot acc curves):
  long_maxN_timespans$metric_pose <- paste0(long_maxN_timespans$metric, sep = "_",
                                            long_maxN_timespans$Pose)


  # plot:
  plot_timespan <- ggplot2::ggplot(data = long_maxN_timespans, group = metric_pose) +

    ggplot2::geom_point(ggplot2::aes(x = time_span, y = values, colour = metric,
                                     fill = metric, alpha = metric, shape = Pose,
                                     size = metric)) +

    ggplot2::geom_line(ggplot2::aes(x = time_span, y = values,
                                    linetype = metric_pose,
                                    color = metric),
                                    size = 0.8) +

    ggplot2::scale_linetype_manual(values = c("dotdash", "dashed", "dotted",
                                              "dotdash", "dashed", "dotted")) +

    ggplot2::scale_fill_manual(values = colors,
                               name = "Metric") +

    ggplot2::scale_colour_manual(values = colors,
                                 name = "Metric") +

    ggplot2::scale_alpha_manual(values = alpha,
                                labels = NULL) +

    ggplot2::scale_shape_manual(values = shape,
                                name = "Pose number") +

    ggplot2::scale_size_manual(values = size,
                               name = "Metric") +

    ggplot2::scale_x_continuous(breaks = c(600, 1200, 1800, 2400, 3000, 3600)) +

    ggplot2::facet_wrap(~ species_nm, ncol = 3,
                        labeller = ggplot2::labeller(species_nm = sp_labs),
                        scales = "free") +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey80"),
                   panel.grid.major = ggplot2::element_line(colour = "grey80")) +

    ggplot2::guides(alpha = "none", size = "none", linetype = "none") +

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



#' Create a dataframe with the minimal time it takes to achieve the maximal SmaxN value
#'
#' This function computes a dataframe with the minimal time it takes to achieve the
#' maximal SmaxN value for each species and each pose and associated plot if asked
#'
#' @param final_timespan_df a dataframe from the \code{clean.df.combcam.maxN}
#' function, type = timespan.
#'
#' @param plot a logical value indicating whether the associated plot should be
#' computed.
#'
#' @param colors a vector containing the different colors for the poses (if
#' not plot should be computed: NA)
#'
#' @return a dataframe with four columns (species, pose, time_to_max, SmaxN) and
#' if asked the associated plot
#'
#' @export
#'

time.to.max <- function(final_timespan_df, plot, colors) {


  # create big df which contains the data:
  time_to_max_df <- as.data.frame(matrix(ncol = 4, nrow = 1))
  colnames(time_to_max_df) <- c("species_nm", "Pose", "time_to_max", "SmaxN")


  for (j in (c("AcCten_dark", "Chaetodon_trifasciatus", "Gomphosus_caeruleus",
               "Parupeneus_macronemus", "Parapercis_hexophtalma",
               "Thalassoma_hardwicke"))) {

    sp_data <- dplyr::filter(final_timespan_df, species_nm %in% c(j))

    for (i in (c("Pose_1", "Pose_2", "Pose_3"))) {

      maxP1 <- max(sp_data$SmaxN[sp_data$Pose == i])
      timeP1 <- sp_data$time_span[sp_data$Pose == i & sp_data$SmaxN == maxP1]
      timeP1 <- min(as.numeric(timeP1))
      time_to_max_df <- dplyr::add_row(time_to_max_df,
                                       species_nm = j,
                                       Pose = i,
                                       time_to_max = timeP1,
                                       SmaxN = maxP1)

    }

  }

  time_to_max_df <- time_to_max_df[-1, ]


  if (plot == TRUE) {

    # factorise poses:
    time_to_max_df$Pose <- as.factor(time_to_max_df$Pose)

    # factorise species:
    time_to_max_df$species_nm <- as.factor(time_to_max_df$species_nm)

    # rename columns with AcCtendark to be Ctenochaetus striatus ...
    # ... so Chaetodon is the first species and it helps for the graph:
    ac_cten <- time_to_max_df[which(time_to_max_df$species_nm == "AcCten_dark"), ]
    ac_cten$species_nm <- rep("Ctenochaetus_striatus", 3)
    time_to_max_df <- time_to_max_df[which(! time_to_max_df$species_nm == "AcCten_dark"), ]
    time_to_max_df <- rbind(time_to_max_df, ac_cten)

    time_to_max_df$species_nm <- forcats::fct_relevel(time_to_max_df$species_nm, c("Chaetodon_trifasciatus",
                                                                       "Ctenochaetus_striatus",
                                                                       "Gomphosus_caeruleus",
                                                                       "Parapercis_hexophtalma",
                                                                       "Parupeneus_macronemus",
                                                                       "Thalassoma_hardwicke"))


    # plot for delta 1:
    plot_max <- ggplot2::ggplot(data = time_to_max_df,
                                   ggplot2::aes(x = species_nm, y = time_to_max)) +

      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(),
                        ggplot2::aes(fill = Pose, colour = Pose),
                        width = 0.40) +

      ggplot2::scale_fill_manual(values = colors,
                                 name = "Pose number") +

      ggplot2::scale_colour_manual(values = colors,
                                   name = "Pose number") +

      ggplot2::scale_alpha_manual(labels = NULL) +

      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                     panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey"),
                     panel.grid.major = ggplot2::element_line(colour = "grey")) +

      ggplot2::scale_y_continuous(limits = c(0, 4200),
                                  breaks = c(600, 1200, 1800, 2400, 3000, 3600, 4200)) +

      ggplot2::scale_x_discrete(labels= c("Chaetodon trifasciatus",
                                          "Ctenochaetus striatus",
                                          "Gomphosus caeruleus",
                                          "Parapercis hexophtalma",
                                          "Parupeneus macronemus",
                                          "Thalassoma hardwicke")) +

      ggplot2::xlab("") +

      ggplot2::ylab("Time to the maximal SmaxN value")


    # save in outputs:
    # save the plot in the outputs folder:
    ggplot2::ggsave(filename = here::here("outputs/4_B_time_to_max.pdf"),
                    plot = plot_max,
                    device = "pdf",
                    scale = 1,
                    height = 5000,
                    width = 5500,
                    units = "px",
                    dpi = 600)
  }

  if (plot == TRUE) {
    return(list(time_to_max_df, plot_max))
  }

  if (plot == FALSE) {
    return(time_to_max_df)
  }

}

