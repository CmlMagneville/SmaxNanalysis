###########################################################################
##
## Script to prepare data and plot the evolution of SmaxN across an increasing
## number of cameras for the three poses altogether(3h timespan)
##
## 6_SmaxN_across_camnb_functions.R
##
## 07/03/2022
##
## Camille Magneville
##
###########################################################################




#' Create list of abundance dataframes with all cameras
#'
#' This function computes a list of abundance dataframes for each species.
#' For each species, the dataframes contain the
#' full number of camers (ie 12)
#'
#' @param species_set a vector containing the names species to study (latin
#' names with no spaces and underscores "_" between Genera and species names)
#'
#' @param cam_set a vector containing the names of cameras to use (camera names
#' should be written as caracter stings)
#'
#' @param abund_list a list gathering the abundance dataframes (one for each
#' pose) and the whole number of cameras
#'
#' @return a list with one dataframe per species
#'
#' @export
#'

create.abundlist.allcam.poses <- function(cam_set,
                                species_set,
                                abund_list) {


  # only select species of species_set:
  clean_abund_list <- abund_list[which(names(abund_list) %in% species_set)]

  # create a new abund_list that will contain df with 12 columns:
  final_abund_list <- list()

  # loop on species:
  for (i in (1:length(clean_abund_list))) {

    print(paste0(names(clean_abund_list)[i], sep = " ", "starts"))

    # create a new list for species with df with right nb of columns (12):
    final_abund_sp_list <- list()

    # loop on the different poses to add column if needed:
    for (j in (1:length(clean_abund_list[[i]]))) {

      print(paste0("Pose", sep = " ", j, sep = " ", "starts"))

      # get the studied df:
      data <- clean_abund_list[[i]][[j]]

      # if not the right number of columns:
      if (ncol(data) != 12) {

        # get the names and nb of columns missing:
        coln <- colnames(data)
        right_coln <- c("I", "H", "G", "F", "E", "D", "C2", "C1",
                        "B2", "B1", "A2", "A1")
        missing <- right_coln[which(! right_coln %in% coln)]

        # for each missing column:
        for (k in (1:length(missing))) {

          # add new column at the end of the df:
          data$new_cam <- rep(0, nrow(clean_abund_list[[i]][[j]]))

          # rename the new column:
          colnames(data)[ncol(data)] <- missing[k]

        } # end add and fill new column

        # reorder the columns:
        data <- data[, right_coln]

      } # end if not the right number of columns


      # fill the species list with new df (transformed if not enough columns or not):
      final_abund_sp_list <- rlist::list.append(final_abund_sp_list, data)

    } # end loop on poses


    # change abundance dataframes in the list so they have all ...
    # ... cameras:
    final_abund_list <- rlist::list.append(final_abund_list, final_abund_sp_list)
    names(final_abund_list)[i] <- names(clean_abund_list[i])

  } # end loop on species

  print(paste0(names(clean_abund_list)[i], sep = " ", "ends"))

  # save the df which as for each species the abundance df with all cam ...
  # ... and poses fusioned:
  saveRDS(final_abund_list, here::here("transformed_data", "abund_list_allcam.rds"))

  # return the final list:
  return(final_abund_list)

}




#' Create a list containing for each species, the abundance dataframes for all
#' cameras combinations
#'
#' This function computes a list which contains for each species a list of
#' abundance dataframes with more or less columns (ie cameras) given the
#' combination of cameras it has been build for
#'
#' @param cam_set a vector containing the names of cameras to use (camera names
#' should be written as caracter stings)
#'
#' @param abund_all_cam_list a list gathering the abundance of each species in a
#' given dataframes with the 12 cameras. One dataframe for one Pose. Retrieved from the
#' create.abundlist.allcam.poses() function.
#'
#' @return a list containing for each species, dataframes corresponding to the
#' combination of the different cameras
#'
#' @export
#'


create.abund.list.camcombn <- function(cam_set,
                                       abund_allcam_list) {

  # compute all the possible combinations of cameras:
  comb_cam <- compute.comb(cam_set)

  # create three lists that will contain for each species as many abundance df ...
  # ... as there are combinations of cameras. One list for one pose:
  final_list_pose1 <- list()
  final_list_pose2 <- list()
  final_list_pose3 <- list()

  # loop on the different species:
  for (i in (1:length(abund_allcam_list))) {


    print(paste0(names(abund_allcam_list)[i], sep = " ", "starts"))


    # create a list that will contain the df for the studied species:
    # ... one for each pose:
    final_sp_list_pose1 <- list()
    final_sp_list_pose2 <- list()
    final_sp_list_pose3 <- list()


    # loop on the poses:
    for (k in (1:length(abund_allcam_list[[i]]))) {

      # retrieve the studied abundance df (for the studied species and pose):
      data <- abund_allcam_list[[i]][[k]]

      # loop on the different combinations:
      for (j in (1:length(comb_cam))) {

        print(paste0("Combination", sep = " ", j, sep = " ", "starts"))


        # get the names of the camera(s) for the studied combination:
        cam_nm <- comb_cam[[j]]

        # only keep columns (= cameras) which are in the combination:
        data2 <- as.data.frame(data[, which(colnames(data) %in% cam_nm)])
        colnames(data2) <- cam_nm

        # add the df with studied cameras to the species list according to
        # ... the studied pose:

        # if pose 1:
        if (k == 1) {
          final_sp_list_pose1 <- rlist::list.append(final_sp_list_pose1, data2)
        }

        # if pose 2:
        if (k == 2) {
          final_sp_list_pose2 <- rlist::list.append(final_sp_list_pose2, data2)
        }

        # if pose 3:
        if (k == 3) {
          final_sp_list_pose3 <- rlist::list.append(final_sp_list_pose3, data2)
        }


      } # end loop on combinations

      # add the list of the given species to the final list according to the ...
      # ... pose number:

      # if pose 1:
      if (k == 1) {
        final_list_pose1 <- rlist::list.append(final_list_pose1, final_sp_list_pose1)
        # rename with species name:
        names(final_list_pose1)[i] <- names(abund_allcam_list)[i]
      }

      # if pose 2:
      if (k == 2) {
        final_list_pose2 <- rlist::list.append(final_list_pose2, final_sp_list_pose2)
        # rename with species name:
        names(final_list_pose2)[i] <- names(abund_allcam_list)[i]
      }

      # if pose 3:
      if (k == 3) {
        final_list_pose3 <- rlist::list.append(final_list_pose3, final_sp_list_pose3)
        # rename with species name:
        names(final_list_pose3)[i] <- names(abund_allcam_list)[i]
      }

      print(paste0(k, sep = " ", "ends"))


    } # end loop on poses


    print(paste0(names(abund_allcam_list)[i], sep = " ", "ends"))


  } # end loop on species


  # create a list of poses list:
  final_list <- list(final_list_pose1, final_list_pose2, final_list_pose3)
  names(final_list) <- c("Pose1", "Pose2", "Pose3")

  # save the final_list which contains abundance data for each species ...
  # ... for each camera: NO BECAUSE TO HEAVY TO GO ON GITHUB
  # saveRDS(final_list, here::here("transformed_data", "abund_list_camcomnb.rds"))

  # return:
  return(final_list)

}




#' Create a dataframe containing maxN values for the studied species and
#' the different combinaison of cameras
#'
#' This function computes a dataframes containing maxN,SmaxN, maxN_row,
#' for a given set of species and all the combinaisons of a given set of
#' cameras
#'
#' @param abund_combcam_list a list gathering for a given pose, a list for each species of
#' abundance dataframes for each combination of cameras (298 combinations).
#' Retrieved from the create.abund.list.camcombn() function.
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
#' @param analysis_type type of analysis to do
#' as this function can be used either for the combination of camera analysis
#' or for the timespan analysis. Can be either "combcam" for
#' the analysis of camera combination or "timespan" for the
#' analysis of timespans.
#'
#' @return a dataframe containing for each combination of cameras, maxN values
#'
#' @importFrom magrittr %>%
#'



compute.maxN.combcam <- function(abund_combcam_list,
                                 dist_df,
                                 fish_speed,
                                 analysis_type) {


  # create a dataframe which will cintain maxN values for all sp:
  maxN_all <- as.data.frame(matrix(ncol = 6, nrow = 1))

  if (analysis_type == "combcam") {
    colnames(maxN_all) <- c("species_nm", "cam_nb", "comb_nm", "maxN", "SmaxN", "SmaxN_row")
  }

  if (analysis_type == "timespan") {
    colnames(maxN_all) <- c("species_nm", "time_span", "pose_nb", "maxN", "SmaxN", "SmaxN_row")
  }


  # loop on species:
  for (i in (1:length(abund_combcam_list))) {

    print(paste0(names(abund_combcam_list[i]), sep = " ", "starts"))


    # create a dataframe which will contain maxN data for the studied sp:
    maxN_sp <- as.data.frame(matrix(ncol = 6, nrow = 1))

    if (analysis_type == "combcam") {
      colnames(maxN_sp) <- c("species_nm", "cam_nb", "comb_nm", "maxN", "SmaxN", "SmaxN_row")
    }

    if (analysis_type == "timespan") {
      colnames(maxN_sp) <- c("species_nm", "time_span", "pose_nb", "maxN", "SmaxN", "SmaxN_row")
    }

    # loop on the different combinaisons:
    for (j in (1:length(abund_combcam_list[[i]]))) {

      print(paste0("Combinaison", sep = " ", j, p = " ", "starts"))


      # compute maxN values (be careful that dist_df has the right nb columns):
      data_dist <- as.data.frame(dist_df[which(rownames(dist_df) %in% colnames(abund_combcam_list[[i]][[j]])),
                                         which(colnames(dist_df) %in% colnames(abund_combcam_list[[i]][[j]]))])
      colnames(data_dist) <- colnames(abund_combcam_list[[i]][[j]])
      rownames(data_dist) <- colnames(abund_combcam_list[[i]][[j]])
      maxN_data <- SmaxN::compute.max.abund(dist_df = data_dist,
                                            fish_speed = fish_speed,
                                            abund_df = abund_combcam_list[[i]][[j]])


      # put maxN values in the maxN_sp df:
      if (analysis_type == "combcam") {
        new_row <- tibble::tibble(species_nm = names(abund_combcam_list[i]),
                                  cam_nb = length(names(abund_combcam_list[[i]][[j]])),
                                  comb_nm = paste(names(abund_combcam_list[[i]][[j]]), collapse = '_'),
                                  maxN = maxN_data$maxN,
                                  SmaxN = maxN_data$SmaxN,
                                  SmaxN_row = maxN_data$SmaxN_row)
      }

      if (analysis_type == "timespan") {
        new_row <- tibble::tibble(species_nm = names(abund_combcam_list[i]),
                                  time_span = stringr::str_sub(names(abund_combcam_list[[i]])[j], 1, 4),
                                  pose_nb = stringr::str_sub(names(abund_combcam_list[[i]])[j], -2, -1),
                                  maxN = maxN_data$maxN,
                                  SmaxN = maxN_data$SmaxN,
                                  SmaxN_row = maxN_data$SmaxN_row)
      }

      maxN_sp <- dplyr::add_row(maxN_sp, new_row)

      print(paste0("Combinaison", sep = " ", j, p = " ", "ends"))


    }

    maxN_all <- dplyr::bind_rows(maxN_all, maxN_sp)

    print(paste0(names(abund_combcam_list[i]), sep = " ", "ends"))

  }

  # save the maxN_all df:
  if (analysis_type == "combcam") {
    saveRDS(maxN_all, here::here("transformed_data", "maxN_combcam.rds"))
  }
  if (analysis_type == "timespan") {
    saveRDS(maxN_all, here::here("transformed_data", "maxN_timespans.rds"))
  }

  # return
  return(maxN_all)

}



#' Create a dataframe containing maxN values for the studied species and
#' the different combinaison of cameras and the different poses
#'
#' This function computes a dataframes containing maxN,SmaxN, maxN_row,
#' for a given set of species, all the combinaisons of a given set of
#' cameras and the different poses.
#'
#' @param abund_poses_combcam_list a list gathering for all poses, a list for each species of
#' abundance dataframes for each combination of cameras (298 combinations).
#' Retrieved from the create.abund.list.camcombn() function. and looks like:
#' pose1 > pose1_sp1 > pose1_sp1_comb1 , pose1 > pose1_sp1 > pose1_sp1_comb2,
#' ... , pose1 > pose1_sp2 > pose1_sp2_comb1, ...,
#' ... , pose2 > pose2_sp1 > pose2_sp1_comb1 etc.
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
#' @param analysis_type type of analysis to do
#' as this function can be used either for the combination of camera analysis
#' or for the timespan analysis. Can be either "combcam" for
#' the analysis of camera combination or "timespan" for the
#' analysis of timespans.
#'
#' @return a dataframe containing for each combination of cameras, maxN values
#'
#' @importFrom magrittr %>%
#'
#' @export
#'


final.combcam <- function(abund_combcam_list, dist_df,
                          fish_speed, analysis_type) {

  # for each list referring to a pose, run the compute.maxN.combcam() fct:

  for (n in (1:length(abund_combcam_list))) {

    #  if pose 1:
    if (n == 1) {
      combcam_pose1 <- compute.maxN.combcam(abund_combcam_list = abund_combcam_list[[n]],
                                           dist_df = dist_df,
                                           fish_speed = fish_speed,
                                           analysis_type = "combcam")
    }

    #  if pose 2:
    if (n == 2) {
      combcam_pose2 <- compute.maxN.combcam(abund_combcam_list = abund_combcam_list[[n]],
                                            dist_df = dist_df,
                                            fish_speed = fish_speed,
                                            analysis_type = "combcam")
    }

    #  if pose 2:
    if (n == 3) {
      combcam_pose3 <- compute.maxN.combcam(abund_combcam_list = abund_combcam_list[[n]],
                                            dist_df = dist_df,
                                            fish_speed = fish_speed,
                                            analysis_type = "combcam")
    }

  } # end loop on the 3 poses

  # add column to combcam_posen to add pose number:

  ## pose1:
  combcam_pose1_final <- combcam_pose1
  combcam_pose1_final$Pose_nb <- "Pose_1"

  ## pose2:
  combcam_pose2_final <- combcam_pose2
  combcam_pose2_final$Pose_nb <- "Pose_2"

  ## pose3:
  combcam_pose3_final <- combcam_pose3
  combcam_pose3_final$Pose_nb <- "Pose_3"


  # bind the three df:
  final_combcam <- dplyr::bind_rows(combcam_pose1_final, combcam_pose2_final,
                             combcam_pose3_final)


  # save the final df:
  saveRDS(final_combcam, here::here("transformed_data", "final_combcam.rds"))

  # return the final df:
  return(final_combcam)

}



#' Create the plot showing SmaxN values for an increasing number of cameras
#'
#' This function computes plots showing SmaxN and maxN values across an
#' increasing number of cameras for 3h for each species (one plot = one sp)
#'
#' @param maxN_combcam a dataframe containing all maxN values, deltas, species
#' names and poses (obtained from automat.maxN.setsp() function)
#'
#' @param colors a vector containing the different colors for SmaxN and maxN
#'
#' @return a ggplot2 object showing the plots of SmaxN and maxN for each
#' species and this plot is saved as pdf in the output folder
#'
#' @export
#'



combcam.plot <- function(maxN_combcam, colors, alpha, shape, size) {

  # numerise cameras:
  maxN_combcam$cam_nb <- as.numeric(maxN_combcam$cam_nb)

  # factorise pose_nb:
  maxN_combcam$Pose_nb <- as.factor(maxN_combcam$Pose_nb)

  # remove NA rows:
  maxN_combcam <- maxN_combcam[which(! is.na(maxN_combcam$species_nm)), ]

  # make in long format:
  long_maxN_combcam <- reshape2::melt(maxN_combcam[, - (ncol(maxN_combcam) - 1)],
                                      id.vars = c("species_nm", "cam_nb", "comb_nm", "Pose_nb"),
                                      variable.name = 'metric', value.name = 'values')

  # create a list of new labels for species:
  sp_labs <- c("C. auriga", "C. trifasciatus", "G. caeruleus",
               "O. longirostris", "P. hexophtalma",
               "P. macronemus", "T. hardwicke")
  names(sp_labs) <- c("Chaetodon_auriga", "Chaetodon_trifasciatus", "Gomphosus_caeruleus",
                      "Oxymonacanthus_longirostris", "Parapercis_hexophtalma",
                      "Parupeneus_macronemus", "Thalassoma_hardwicke")

  # create a list of new labels for poses:
  pose_labs <- c("Pose 1: 7:30-8:30", "Pose 2: 11:30-12:30", "Pose3: 15:30-16:30")
  names(pose_labs) <- c("Pose_1", "Pose_2", "Pose_3")


  # plot:
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

    ggplot2::facet_grid(cols = ggplot2::vars(Pose_nb),
                        rows = ggplot2::vars(species_nm),
                        labeller = ggplot2::labeller(species_nm = sp_labs,
                                                     Pose_nb = pose_labs)) +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey"),
                   panel.grid.major = ggplot2::element_line(colour = "grey"),
                   strip.text.y = ggplot2::element_text(size = 8)) +

    ggplot2::guides(colour = "none", alpha = "none", size = "none") +

    ggplot2::xlab("Camera number") +

    ggplot2::ylab("SmaxN vs maxN")



  # save in outputs:
  # save the plot in the outputs folder:
  ggplot2::ggsave(filename = here::here("outputs/3_maxN_combcam_vers2.pdf"),
                  plot = plot_combcam,
                  device = "pdf",
                  scale = 1,
                  height = 5000,
                  width = 8000,
                  units = "px",
                  dpi = 600)

  # return the plot:
  return(plot_combcam)


}
