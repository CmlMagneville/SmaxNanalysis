###########################################################################
##
## Script to code functions to manipulate df before computing maxN values
##
## 1_Manipulate_table_function.R
##
## 11/2021
##
## Camille Magneville
##
###########################################################################



#' Create a dataframe with abundance data for different cameras and a given
#' species
#'
#' This function computes a dataframe containing the abundance of a given
#' species for a given period of time. The user gives as many dataframes as
#' there are cameras, a period of time and a species name and the function
#' checks on which cameras the species has been seen and gathers the dataframes
#' of these cameras.
#'
#' @param list_abund_df a list containing as many dataframes as there are
#' cameras. Each dataframe is a species*abundance dataframe and contains the
#' abundance of different species (columns) across continuous time (rows)
#'
#' @param species_nm a character string refering to the name of the species
#' for which dataframes must be gathered. \strong{Be sure that species names
#' is correctly spelled as in the vect_abund_df dataframes}.
#'
#' @param time_start a character string refering to the time when the dataframe
#' should begin.
#'
#' @param time_stop a character string refering to the time when the dataframe
#' should stop.
#'
#' @return the function returns a dataframe containing the abundance of a given
#' species for a given period of time. There are as many columns as there are
#' cameras and there are as many rows as there are timesteps (second) between
#' time_start and time_stop.
#'


gather.abund.df <- function(list_abund_df, species_nm,
                            time_start, time_stop) {


  ## 0 - check that the species names exists in at least one abundance df:

  # create a vector with the names of species present in all the df:
  sp_nm_df <- c()

  for (i in list_abund_df) {
    sp_nm_df <- append(colnames(i), sp_nm_df)
  }

  sp_nm_df <- unique(sp_nm_df)

  # check if studied species in it:
  if (! species_nm %in% sp_nm_df) {
    stop("Species in species_nm is not contained in the dataframes of list_abund_df.
         Please check, it may be mispelled.")
  }


  ## 1 - cut the df between time_start and time_stop:

  # create a vector that contains all seconds between time_start and time_stop:
  time_vect <- hms::as_hms(c(hms::as_hms(time_start):hms::as_hms(time_stop)))
  time_vect <- as.character(time_vect)

  # only keep rows in the list_abund_df that span in time_vect and keep the given sp:
  for (i in c(1:length(list_abund_df))) {
    list_abund_df[[i]] <- list_abund_df[[i]][which(rownames(list_abund_df[[i]]) %in% time_vect), ]
  }


  ## 2 - only keep df that present the studied sp:

  # create a vector that will contain the names of df containing the species
  cam_keep <- c()

  # fill the vector with df containing the studied species:
  for (i in c(1:length(list_abund_df))) {
    if (species_nm %in% colnames(list_abund_df[[i]])) {
      cam_keep <- append(list_abund_df[i], cam_keep)
    }
  }


  ## 3 - Combine df:

  # first add the 1st column:
  data <- data.frame(cam_keep[[1]][, species_nm])
  rownames(data) <- rownames(cam_keep[[1]])

  # then, if more than one camera (df in cam_keep) to keep:
  if (length(cam_keep) > 1) {
    for (i in c(2:length(cam_keep))) {
      df <- as.data.frame(cam_keep[[i]][, species_nm])
      rownames(df) <- rownames(cam_keep[[i]])
      data <- dplyr::bind_cols(data, df)
    }
  }

  # add columns names as camera names:
  colnames(data) <- names(cam_keep)


  ## 4 - Return
  return(list(abund_df = data,
              cam_nm = names(cam_keep)))

}
