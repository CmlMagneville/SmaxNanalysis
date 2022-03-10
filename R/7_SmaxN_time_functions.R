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


      # create a list that will contains new data for a given pose
      abund_time_pose_list <- list()


      # loop on the timespans:
      for (k in (1:length(spans_set))) {




      } # end loop on tiemspans

    } # end loop on poses


  } # end loop on species



}
