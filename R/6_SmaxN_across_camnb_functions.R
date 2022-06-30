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
#' @param species_nm a character string containing the name species to study (latin
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
                                species_nm,
                                abund_list) {


  # only select species of species_set:
  clean_abund_list <- abund_list[which(names(abund_list) %in% species_nm)]

  # create a new abund_list that will contain df with 9 (ICRS) 12 (paper) columns:
  final_abund_list <- list()

  # loop on poses:
  for (i in (1:length(clean_abund_list[[1]]))) {

    print(paste0("Pose", sep = " ", i, sep = " ", "starts"))

    # get the studied df:
    data <- clean_abund_list[[1]][[i]]

    # if not the right number of columns: CHANGE HERE PAPER
    if (ncol(data) != 9) {

      # get the names and nb of columns missing: CHANGE HERE PAPER
      coln <- colnames(data)
      right_coln <- c("H", "F", "D", "C2", "C1",
                      "B2", "B1", "A2", "A1")
      missing <- right_coln[which(! right_coln %in% coln)]

      # for each missing column:
      for (k in (1:length(missing))) {

        # add new column at the end of the df:
        data$new_cam <- rep(0, nrow(clean_abund_list[[1]][[i]]))

        # rename the new column:
        colnames(data)[ncol(data)] <- missing[k]

      } # end add and fill new column

      # reorder the columns:
      data <- data[, right_coln]

    } # end if not the right number of columns


    # fill the species list with new df (transformed if not enough columns or not):
    final_abund_list <- rlist::list.append(final_abund_list, data)
    names(final_abund_list)[i] <- paste0("Pose", sep = "_", i)


  } # end loop on poses


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
#' @param abund_all_cam_list a list gathering the abundance of the studied species in a
#' given dataframes with the 12 cameras. One dataframe for one Pose. Retrieved from the
#' create.abundlist.allcam.poses() function.
#'
#' @return a list containing the dataframes corresponding to the
#' combination of the different cameras
#'
#' @export
#'


create.abund.list.camcombn <- function(cam_set,
                                       abund_allcam_list) {

  # compute all the possible combinations of cameras:
  comb_cam <- compute.comb(cam_set)

  # create three lists that will contain as many abundance df ...
  # ... as there are combinations of cameras. One list for one pose:
  final_list_pose1 <- list()
  final_list_pose2 <- list()
  final_list_pose3 <- list()


  # loop on the poses:
  for (k in (1:length(abund_allcam_list))) {

    # retrieve the studied abundance df (for the studied species and pose):
    data <- abund_allcam_list[[k]]

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
        final_list_pose1 <- rlist::list.append(final_list_pose1, data2)
      }

      # if pose 2:
      if (k == 2) {
        final_list_pose2 <- rlist::list.append(final_list_pose2, data2)
      }

      # if pose 3:
      if (k == 3) {
        final_list_pose3 <- rlist::list.append(final_list_pose3, data2)
      }


    } # end loop on combinations

    print(paste0(k, sep = " ", "ends"))


  } # end loop on poses

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
#' @param abund_combcam_list a list gathering for a given pose
#'  for each combination of cameras (129 ICRS and 298 combinations paper).
#'  Retrieved from the create.abund.list.camcombn() function.
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


  if (analysis_type == "combcam") {
    maxN_all <- as.data.frame(matrix(ncol = 5, nrow = 1))
    colnames(maxN_all) <- c("cam_nb", "comb_nm", "maxN", "SmaxN", "SmaxN_timestep")
  }

  if (analysis_type == "timespan") {
    maxN_all <- as.data.frame(matrix(ncol = 4, nrow = 1))
    colnames(maxN_all) <- c("time_span", "maxN", "SmaxN", "SmaxN_timestep")
  }


  # loop on poses:
  for (i in (1:length(abund_combcam_list))) {

    print(paste0(names(abund_combcam_list[i]), sep = " ", "starts"))

    # loop on the different combinaisons:
    for (j in (1:length(abund_combcam_list[[i]]))) {

      print(paste0("Combinaison", sep = " ", j, p = " ", "starts"))


      # compute maxN values (be careful that dist_df has the right nb columns):
      data_dist <- as.data.frame(dist_df[which(rownames(dist_df) %in% colnames(abund_combcam_list[[i]][[j]])),
                                         which(colnames(dist_df) %in% colnames(abund_combcam_list[[i]][[j]]))])
      colnames(data_dist) <- colnames(abund_combcam_list[[i]][[j]])
      rownames(data_dist) <- colnames(abund_combcam_list[[i]][[j]])
      maxN_data <- SmaxN::SmaxN.computation(dist_df = data_dist,
                                            speed = fish_speed,
                                            abund_df = abund_combcam_list[[i]][[j]])

      print("maxN computed")

      # put maxN values in the maxN_sp df:
      if (analysis_type == "combcam") {
        new_row <- tibble::tibble(cam_nb = length(names(abund_combcam_list[[i]][[j]])),
                                  comb_nm = paste(names(abund_combcam_list[[i]][[j]]), collapse = '_'),
                                  maxN = maxN_data$maxN,
                                  SmaxN = maxN_data$SmaxN,
                                  SmaxN_timestep = maxN_data$SmaxN_timestep)
      }

      if (analysis_type == "timespan") {
        new_row <- tibble::tibble(time_span = stringr::str_sub(names(abund_combcam_list[[i]])[j], 1, 4),
                                  maxN = maxN_data$maxN,
                                  SmaxN = maxN_data$SmaxN,
                                  SmaxN_timestep = maxN_data$SmaxN_timestep)
      }

      maxN_all <- dplyr::add_row(maxN_all, new_row)

      print(paste0("Combinaison", sep = " ", j, p = " ", "ends"))


    }

    print(paste0(names(abund_combcam_list[i]), sep = " ", "ends"))

  }

  # return
  return(maxN_all)

}




#' Create a dataframe containing SmaxN values for all studied species and
#' combinaison of cameras
#'
#' This function computes a dataframes containing maxN,SmaxN, maxN_row,
#' for all species and the combination of cameras.
#'
#' @param all_sp_list a list containing for each species the output of the
#' compute.maxN.combcam() function. Names must be given for each
#' species. See script 4.SmaxN_increasing_nbcam_analysis.R part 3
#' "arrange data".
#'
#' @param type indicating whther the time or the combcam analysis is realised:
#' can have two values "combcam" if we study the evolution of SmaxN and maxN
#' over an increasing number of cameras and "timespan" if we study the evolution
#' of SmaxN and maxN over an increasing recording time.
#'
#' @return a dataframe with six columns: species_nm, cam_nb, comb_nm, maxN, SmaxN
#' and SmaxN_timestep
#'
#' @importFrom magrittr %>%
#'
#' @export
#'


clean.df.combcam.maxN <- function(all_sp_list, type) {


  # create the df if type combcam:
  if (type == "combcam") {
    # create a dataframe that will contains all value to plot:
    maxN_combcam <- as.data.frame(matrix(ncol = 6, nrow = 1))
    colnames(maxN_combcam) <- c("species_nm", "cam_nb", "comb_nm", "maxN", "SmaxN", "SmaxN_timestep")
  }

  # create the df if type timespan
  if (type == "timespan") {
    # create a dataframe that will contains all value to plot:
    maxN_combcam <- as.data.frame(matrix(ncol = 5, nrow = 1))
    colnames(maxN_combcam) <- c("species_nm", "time_span", "maxN", "SmaxN", "SmaxN_timestep")
  }

  # loop on the species list:
  for (i in (1:length(all_sp_list))) {

    sp_nm <- names(all_sp_list)[i]
    data <- as.data.frame(all_sp_list[[i]])
    data <- data[-1, ]


    # if type == combcam, correct for cam = 1:
    if (type == "combcam") {

      # if the number of cam = 1, SmaxN and SmaxN_timestep = NA because rely on ...
      # ... the use of several cameras:
      # here I will set their value to the maxN value:
      for (j in (1:nrow(data))) {

        if (data$cam_nb[j] == 1) {
          data$SmaxN[j] <- data$maxN[j]
          data$SmaxN_timestep[j] <- data$maxN[j]
        }

      }

    }

    # complete the df if type combcam:
    if (type == "combcam") {
      # create a df to add to the maxN_combcam df:
      species_nm <- rep(sp_nm, nrow(data))
      cam_nb <- data$cam_nb
      comb_nm <- data$comb_nm
      maxN <- data$maxN
      SmaxN <- data$SmaxN
      SmaxN_timestep <- data$SmaxN_timestep

      data_to_add <- data.frame(species_nm, cam_nb, comb_nm, maxN,
                                SmaxN, SmaxN_timestep)
    }

    # complete the df if type combcam:
    if (type == "timespan") {
      # create a df to add to the maxN_combcam df:
      species_nm <- rep(sp_nm, nrow(data))
      time_span <- data$time_span
      maxN <- data$maxN
      SmaxN <- data$SmaxN
      SmaxN_timestep <- data$SmaxN_timestep

      data_to_add <- data.frame(species_nm, time_span, maxN,
                                SmaxN, SmaxN_timestep)
    }



    # add the df of the studied species to the maxNcombcam df:
    maxN_combcam <- rbind(maxN_combcam, data_to_add)

  }


  # now add the Pose number which has been forgotten in the maxN.combcam fct:
  # for each species: the 129 first rows are Pose 1, then Pose 2 then Pose 3:

  # add new column:
  maxN_combcam$Pose <- rep(NA, nrow(maxN_combcam))

  # remove first NA row:
  maxN_combcam <- maxN_combcam[-1, ]

  for (i in (1:length(unique(maxN_combcam$species_nm)))) {

    sp_nm <- as.character(unique(maxN_combcam$species_nm)[i])

    data_sp <- maxN_combcam[which(maxN_combcam$species_nm == sp_nm), ]

    if (type == "timespan") {

      # Pose 1 (6 because 6 timespans):
      data_sp[c(1:6), "Pose"] <- "Pose_1"

      # Pose 2:
      data_sp[c(7:12), "Pose"] <- "Pose_2"

      # Pose 3:
      data_sp[c(13:nrow(data_sp)), "Pose"] <- "Pose_3"

    }

    if (type == "combcam") {

      # Pose 1 (129 because 9 cameras and 129 combinations):
      data_sp[c(1:129), "Pose"] <- "Pose_1"

      # Pose 2:
      data_sp[c(130:258), "Pose"] <- "Pose_2"

      # Pose 3:
      data_sp[c(259:nrow(data_sp)), "Pose"] <- "Pose_3"

    }



    # collide data_sp in maxN_combcam:
    maxN_combcam[which(maxN_combcam$species_nm == sp_nm), ] <- data_sp

  }

  return(maxN_combcam)

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



combcam.plot <- function(maxN_combcam, colors, alpha, shape, size, compare) {

  # numerise cameras:
  maxN_combcam$cam_nb <- as.numeric(maxN_combcam$cam_nb)

  # factorise pose:
  maxN_combcam$Pose <- as.factor(maxN_combcam$Pose)

  # factorise species:
  maxN_combcam$species_nm <- as.factor(maxN_combcam$species_nm)


  # make in long format:
  long_maxN_combcam <- reshape2::melt(maxN_combcam,
                                      id.vars = c("species_nm", "cam_nb", "comb_nm", "Pose"),
                                      variable.name = 'metric', value.name = 'values')

  # rename AcCten_dark in Ctenochaetus_striatus
  ac_cten <- long_maxN_combcam[which(long_maxN_combcam$species_nm == "AcCten_dark"), ]
  ac_cten$species_nm <- rep("Ctenochaetus_striatus", nrow(ac_cten))
  ac_cten$species_nm <- as.character(ac_cten$species_nm)
  long_maxN_combcam <- long_maxN_combcam[which(! long_maxN_combcam$species_nm == "AcCten_dark"), ]

  long_maxN_combcam <- rbind(long_maxN_combcam, ac_cten)
  long_maxN_combcam$species_nm <- as.factor(long_maxN_combcam$species_nm)
  long_maxN_combcam$species_nm <- forcats::fct_relevel(long_maxN_combcam$species_nm, c("Chaetodon_trifasciatus",
                                              "Ctenochaetus_striatus",
                                              "Gomphosus_caeruleus",
                                              "Parupeneus_macronemus",
                                              "Parapercis_hexophtalma",
                                              "Thalassoma_hardwicke"))


  # remove unwanted rows according to the wanted graph = SmaxN vs maxN or SmaxN vs SmaxN_timestep:
  if (compare == "maxN") {
    long_maxN_combcam <- long_maxN_combcam[which(! long_maxN_combcam$metric == "SmaxN_timestep"), ]
    long_maxN_combcam$metric <- as.factor(long_maxN_combcam$metric)
  }

  if (compare == "SmaxN_timestep") {
    long_maxN_combcam <- long_maxN_combcam[which(! long_maxN_combcam$metric == "maxN"), ]
    long_maxN_combcam$metric <- as.factor(long_maxN_combcam$metric)
  }

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

    ggplot2::scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) +

    ggplot2::facet_grid(cols = ggplot2::vars(Pose),
                        rows = ggplot2::vars(species_nm),
                        labeller = ggplot2::labeller(species_nm = sp_labs,
                                                     Pose = pose_labs),
                        scales = "free") +

    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                            colour = "grey80"),
                   panel.grid.major = ggplot2::element_line(colour = "grey80"),
                   strip.text.y = ggplot2::element_text(size = 8)) +

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
                  width = 8000,
                  units = "px",
                  dpi = 600)

  # return the plot:
  return(plot_combcam)


}
