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


#' Computes SmaxN and other abundance metrics using a list which have
#' the abundance dataframe, the speed and the distance dataframe as input
#'
#' @param list_input is a list which contains the abundance dataframe of the
#' studied species, the distance dataframe and the speed for a given pose
#'
#' @return the result of the \code{SmaxN.computation} function
#'
#' @export
#'


SmaxN.single.inp <- function(i) {
  SmaxN::SmaxN.computation(dist_df = paral_list[[i]]$dist_df,
                           speed = paral_list[[i]]$speed,
                           abund_df = paral_list[[i]]$abund_df)
}

# Don't use anymore because compute for only one species at a time:
#' #' Create a dataframe containing maxN values for the studied species and
#' #' the three poses
#' #'
#' #' This function computes a dataframes containing maxN,SmaxN, maxN_row,
#' #' for a given set of species and the three poses. This function uses
#' #' parallelisation.
#' #'
#' #' @param species_set a vector containing the latin name of the studied species
#' #' given with no space but underscore "_" between Genera and Species names
#' #'
#' #' @param abund_list a list gathering the abundance dataframes of the different
#' #' Poses (dataframes with cameras = columns and time = rows)
#' #'
#' #' @param dist_df  a numerical dataframe containing the distance between each
#' #'  pair of camera. There are as many rows as there are cameras and there are
#' #'  as many columns as there are cameras, thus the dataframe is symmetrical
#' #'  and the diagonal is filled with 0. \strong{Rows names and columns names
#' #'  must be cameras names}. \strong{BE CAREFUL that the cameras are
#' #' the same and in the same order in the dist_df and in the abund_df!}
#' #'
#' #' @param fish_speed a numerical value refering to the maximal speed of the
#' #'  studied species. \strong{Speed must be given in meters per second}. If the
#' #'  computation of maxN values should not take into account fish speed (that is
#' #'  to say if the camera pooling is done at the second level),
#' #'  \code{fish_speed = NULL}
#' #'
#' #'  @param os a chacaretr string refering to the operating system used as the
#' #'  parallelisation process differs following the os. can either be "windows" or
#' #'  "linux". (mac not done)
#' #'
#' #' @return a dataframe with five columns: maxN,SmaxN, maxN_row,
#' #' Species_nm, Poses
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @export
#' #'
#'
#'
#'
#'
#' automat.maxN.setsp <- function(species_set, abund_list, dist_df, fish_speed) {
#'
#'
#'   # create a dataframe which will contain all data:
#'   maxN_all <- as.data.frame(matrix(ncol = 5, nrow = 1))
#'   colnames(maxN_all) <- c("species_nm", "pose_nb", "maxN", "SmaxN", "SmaxN_timestep")
#'
#'
#'   # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores:
#'   no_cores <- parallel::detectCores(logical = TRUE)
#'
#'   # allocate this nb of available cores to R:
#'   cl <- parallel::makeCluster(no_cores-1)
#'   doParallel::registerDoParallel(cl)
#'
#'
#'
#'   # restrict the abund_list to the studied species:
#'   clean_abund_list <- abund_list[which(names(abund_list) %in% species_set)]
#'
#'
#'   # loop on species:
#'   for (i in (1:length(clean_abund_list))) {
#'
#'     # create a dataframe which will contain maxN data for the studied sp:
#'     maxN_sp <- as.data.frame(matrix(ncol = 5, nrow = 1))
#'     colnames(maxN_sp) <- c("species_nm", "pose_nb", "maxN", "SmaxN", "SmaxN_timestep")
#'
#'
#'       # create the list containing the objects on which to parallise:
#'       # (be careful that dist_df has the right nb columns):
#'
#'       paral_list <- list()
#'
#'       # loop on the different poses:
#'       for (j in (1:length(clean_abund_list[[i]]))) {
#'
#'         paral_list <- append(paral_list, list(list(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[i]][[j]])),
#'                                                                   which(colnames(dist_df) %in% colnames(clean_abund_list[[i]][[j]]))],
#'                                               speed = fish_speed,
#'                                               abund_df = clean_abund_list[[i]][[j]])))
#'
#'       } # end creating the list to parallelise
#'
#'       if (os == "windows") {
#'         start.time <- Sys.time()
#'         maxN_data <- parallel::parLapply(cl, paral_list, fun = SmaxN.single.inp)
#'         end.time <- Sys.time()
#'         time.taken <- end.time - start.time
#'       }
#'
#'       if (os == "linux") {
#'         start.time <- Sys.time()
#'         maxN_data <- parallel::mclapply(paral_list, fun = SmaxN.single.inp)
#'         end.time <- Sys.time()
#'         time.taken <- end.time - start.time
#'       }
#'
#'       # put maxN values in the maxN_sp df for the three poses:
#'       for (j in (1:length(maxN_data))) {
#'
#'         new_row <- tibble::tibble(species_nm = names(clean_abund_list)[i],
#'                                   pose_nb = j,
#'                                   maxN = maxN_data[[j]]$maxN,
#'                                   SmaxN = maxN_data[[j]]$SmaxN,
#'                                   SmaxN_timestep = maxN_data[[j]]$SmaxN_timestep,
#'         )
#'         maxN_sp <- dplyr::add_row(maxN_sp, new_row)
#'
#'       }
#'
#'       # add species data to the global data:
#'       maxN_all <- dplyr::bind_rows(maxN_all, maxN_sp)
#'
#'     } # end loop on species
#'
#'
#'    # remove NA rows:
#'    maxN_all <- maxN_all[which(! is.na(maxN_all$species_nm)), ]
#'
#'    # Close Cluster:
#'    stopCluster(cl)
#'
#'   # return global maxN:
#'   return(list(maxN_all, time.taken))
#'
#' }



#' Create a dataframe containing maxN values for one studied species and
#' the three poses
#'
#' This function computes a dataframes containing maxN,SmaxN, maxN_row,
#' for a one species and the three poses. This function uses
#' parallelisation.
#'
#' @param species_nm a caracter string containing the latin name of the studied species
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
#'  @param os a chacaretr string refering to the operating system used as the
#'  parallelisation process differs following the os. can either be "windows" or
#'  "linux". (mac not done)
#'
#'  @param nb_cores is a nuemric value refering to the number of cores on which
#'  the process should be parallelised (do not need more than 3 cores as 3 poses)
#'
#' @return a dataframe with five columns: maxN,SmaxN, maxN_row,
#' Species_nm, Poses
#'
#' @importFrom magrittr %>%
#'
#' @export
#'



automat.maxN.spbysp <- function(species_nm, abund_list, dist_df, fish_speed, os, nb_cores) {

  if (os == "windows") {
    # allocate 3 cores to this process (server has 100 cores and only need 3 for the
    # ... poses):
    cl <- parallel::makeCluster(3)
    doParallel::registerDoParallel(cl)
  }

  # restrict the abund_list to the studied species:
  clean_abund_list <- list()
  clean_abund_list <- abund_list[which(names(abund_list) == species_nm)]

  paral_list <- list()

  # loop on the different poses:
  for (j in (1:length(clean_abund_list[[1]]))) {

    paral_list <- append(paral_list, list(list(dist_df = dist_df[which(rownames(dist_df) %in% colnames(clean_abund_list[[1]][[j]])),
                                                                 which(colnames(dist_df) %in% colnames(clean_abund_list[[1]][[j]]))],
                                               speed = fish_speed,
                                               abund_df = clean_abund_list[[1]][[j]])))

  } # end creating the list to parallelise

  if (os == "windows") {
    start.time <- Sys.time()
    maxN_data <- parallel::parLapply(cl, X = 1:length(paral_list), fun = function(i) {
      SmaxN::SmaxN.computation(dist_df = paral_list[[i]]$dist_df,
                               speed = paral_list[[i]]$speed,
                               abund_df = paral_list[[i]]$abund_df)
    })
    end.time <- Sys.time()
    time.taken <- end.time - start.time
  }

  if (os == "linux") {
    start.time <- Sys.time()
    maxN_data <- parallel::mclapply(X = 1:length(paral_list), FUN = function(i) {
      SmaxN::SmaxN.computation(dist_df = paral_list[[i]]$dist_df,
                               speed = paral_list[[i]]$speed,
                               abund_df = paral_list[[i]]$abund_df)
    },
    mc.cores = nb_cores)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
  }

  # return global maxN:
  return(maxN_data)

}



#' Create a dataframe containing maxN values for one studied species and
#' the three poses
#'
#' This function computes a dataframes containing maxN,SmaxN, maxN_row,
#' for a one species and the three poses. This function uses
#' parallelisation.
#'
#' @param all_sp_list a list containing for each species the output of the
#' automat.maxN.spbysp() function. Names must be given for each
#' species. See script 3.maxN_values_delta_analysis_spbysp.R part 2
#' "arrange data".
#'
#' @return a dataframe with five columns: species_nm, pose_nb, maxN, SmaxN
#' and maxN_timestep
#'
#' @importFrom magrittr %>%
#'
#' @export
#'


clean.df.maxN <- function(all_sp_list) {

  # create a dataframe that will contains all value to plot:
  maxN_all <- as.data.frame(matrix(ncol = 5, nrow = 1))
  colnames(maxN_all) <- c("species_nm", "pose", "maxN", "SmaxN", "SmaxN_timestep")


  # loop on the species list:
  for (i in (1:length(all_sp_list))) {

    sp_nm <- names(all_sp_list)[i]

    # loop on the Poses:
    for (j in (1:length(all_sp_list[[i]]))) {

      pose_nb <- paste0("Pose", sep = "_", j)

      # get the data for sp 1 and pose j:
      data <- all_sp_list[[i]][[j]]

      # get the values:
      maxN <- data$maxN
      SmaxN <- data$SmaxN
      SmaxN_timestep <- data$SmaxN_timestep

      # new row to add maxN:
      new_row <- tibble::tibble(species_nm = sp_nm,
                                pose = pose_nb,
                                maxN = maxN,
                                SmaxN = SmaxN,
                                SmaxN_timestep = SmaxN_timestep)
      # add:
      maxN_all <- dplyr::add_row(maxN_all, new_row)

    }

  }

  # remove first row with NA:
  maxN_all <- maxN_all[-1, ]

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
  maxN_all$pose <- as.factor(maxN_all$pose)

  # factorise species:
  maxN_all$species_nm <- as.factor(maxN_all$species_nm)

  # if SmaxN and maxN equal 0 then remove rows because otherwise a difference of 0
  # ... can be due to same SmaxN and maxN or to SmaxN = maxN = 0 (no individuals)
  maxN_all <- maxN_all[which(maxN_all$SmaxN != 0), ]

  # compute deltas:
  maxN_all$delta_SmaxN_maxN <- maxN_all$SmaxN / maxN_all$maxN
  maxN_all$delta_SmaxN_SmaxNtimestep <- maxN_all$SmaxN / maxN_all$SmaxN_timestep


  # plot for delta 1:
  plot_delta1 <- ggplot2::ggplot(data = maxN_all,
                                 ggplot2::aes(x = species_nm, y = delta_SmaxN_maxN)) +

    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(),
                      ggplot2::aes(fill = pose, colour = pose),
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

    ggplot2::scale_y_continuous(limits = c(0, 5),
                                breaks = c(0.5, 1, 1.5, 2, 2.5, 3,
                                           3.5, 4, 4.5, 5)) +

    ggplot2::scale_x_discrete(labels= c("Ctenochaetus striatus",
                                      "Chaetodon trifasciatus",
                                      "Gomphosus caeruleus",
                                      "Parupeneus macronemus")) +

    ggplot2::xlab("") +

    ggplot2::ylab("Delta 1 = SmaxN / maxN") +

    fishualize::add_fishape(family = "Acanthuridae",
                            option = "Ctenochaetus_striatus",
                            xmin = 0.5, xmax = 1.5, ymin = 4.1, ymax = 4.9,
                            scaled = FALSE,
                            fill = "grey38",
                            alpha = 0.3)



  # plot for delta 2:
  plot_delta2 <- ggplot2::ggplot(data = maxN_all,
                                 ggplot2::aes(x = species_nm, y = delta_SmaxN_SmaxNtimestep)) +

    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(),
                      ggplot2::aes(fill = pose, colour = pose),
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

    ggplot2::scale_y_continuous(limits = c(0, 4.5),
                                breaks = c(0.5, 1, 1.5, 2, 2.5, 3,
                                           3.5, 4, 4.5)) +

    ggplot2::scale_x_discrete(labels= c("Ctenochaetus striatus",
                                        "Chaetodon trifasciatus",
                                        "Gomphosus caeruleus",
                                        "Parupeneus macronemus")) +

    ggplot2::xlab("") +

    ggplot2::ylab("Delta 2 = SmaxN / SmaxN_timestep") +

    fishualize::add_fishape(family = "Acanthuridae",
                            option = "Ctenochaetus_striatus",
                            xmin = 0.5, xmax = 1.5, ymin = 3.1, ymax = 3.9,
                            scaled = FALSE,
                            fill = "grey38",
                            alpha = 0.3)

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

