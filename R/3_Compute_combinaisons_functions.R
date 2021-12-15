###########################################################################
##
## Script to compute all combinaisons of cameras and give the SmaxN
## value for each combinaison
##
## 3_Compute_combianisons_functions.R
##
## 12/2021
##
## Camille Magneville
##
###########################################################################


#' Create a list containing all combinaisons of n cameras
#'
#' This function computes a list which contains all combinaisons of n
#' cameras: all single cameras, then all pairs of cameras, then
#' all triplets of cameras etc with no repetition. This function is
#' based on:
#' https://yihui.org/cn/2007/04/generate-all-combinations-of-n-elements-in-r/
#'
#' @param vect_cam a vector containing all camera names such as:
#' c("A1", "A2", "B1", "B2", "C1", "C2", "D", "E", "F", "G", "H", "I")
#'
#' @return the function returns a list with all combinaisons
#'
#' @export
#'



compute.comb <- function(vect_cam) {

  n <- length(vect_cam)

  # create a list that will contains all values:
  c <- list()

  # fill it using Yihui function:
  for (i in 1:n) {

    if (i == 1) {

      for (j in 1:n) {

        c <- rlist::list.append(c, vect_cam[j])
      }

    }

    else {

      for (j in 1:(n - i + 1)) {

        for (k in (j + i - 1):n) {

          c <- rlist::list.append(c, c(vect_cam[j:(j + i - 2)], vect_cam[k]))

        }
      }
    }
  }

  return(c)
}
