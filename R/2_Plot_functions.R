###########################################################################
##
## Script to build df to plot maxN values and plot
##
## 1_Manipulate_table_function
##
## 11/2021
##
## Camille Magneville
##
###########################################################################


#' Create a dataframe with maxN data to plot for a given species
#'
#' This function computes a dataframe containing the maxN values for different
#' definition of maxN. The user gives a
#' list of maxN values which is the output of the
#' \code{compute.max.abund() function} of the \code{SmaxN package}.
#' The function returns a df to be used to plot the maxN values.
#'
#' @param list_maxN a list containing maxN values. This list is the ouput of
#'  the \code{compute.max.abund() function} of the \code{SmaxN package}.
#'
#' @param indices_nm a vector containing the names of maxN inidces to plot
#'
#' @return the function returns a dataframe values to plot
#'
#'


prepare.maxN.plot <- function(list_maxN,
                              indices_nm) {


  # create a dataframe that will contain data to plot:
  data_plot <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 3))
  colnames(data_plot) <- c("graph_type", "maxN_index", "values")


  # do a loop on the list_maxN to get values of indices to keep:
  for (i in indices_nm) {


    # get the value(s) of the index i:
    val <- list_maxN[[i]]


    # if the index has several values (for instance maxN across cam or rows):
    if (length(val) > 1) {

      # span each value and put it in the data_plot dataframe...
      # ... by ceatin a vector with all informations:
      # ... if several values then boxplot, index_nm and value:

      for (j in c(1:length(val))) {

        val2 <- val[[j]]
        graph <- "boxplot"
        index <- i

        vect <- c(graph, index, val2)

        # add to the data_plot df:
        data_plot <- rbind(data_plot, vect)

      } # end the loop to span each element of maxN index values

    } # end if several values in the maxN index


    # if the index has one value (for instance maxN or SmaxN):
    if (length(val) == 1) {

      val2 <- val
      graph <- "points"
      index <- i

      vect <- c(graph, index, val2)

      # add to the data_plot df:
      data_plot <- rbind(data_plot, vect)

    } # end if one value for the maxN index

  } # end loop across all index

  # remove the 1st row of data_plot which is filled with NAs:
  data_plot <- data_plot[-1, ]

  # make the graph column a factor
  data_plot$graph_type <- as.factor(data_plot$graph_type)

  # make the value column numeric:
  data_plot$values <- as.numeric(data_plot$values)

  return(data_plot)

} # end function



#############################################################################



#' Plot the maxN values
#'
#' This function computes a plot containing the maxN values for different
#' definition of maxN. The user gives a data.frame which is the output of the
#' \code{prepare.maxN.plot() function}. The function returns the plot.
#'
#' @param maxN_plot_df a dataframe containing maxN values. This dataframe is
#' the ouput of the \code{prepare.maxN.plot() function}.
#'
#' @param color_points a R color code for plotting points
#'
#' @param alpha_points a transparency code for plotting points
#'
#' @param color_boxplot a R color code for plotting boxplot(s)
#'
#' @param alpha_boxplot a transparency code for plotting boxplot(s)
#'
#' @return the function returns a plot of maxN values
#'
#'


plot.maxN <- function(maxN_plot_df,
                      color_points, alpha_points,
                      color_boxplots, alpha_boxplots) {

  # create the plot:

  maxN_plot <- ggplot2::ggplot(data = maxN_plot_df,
                              ggplot2::aes(x = maxN_index, y = values)) +
    ggplot2::geom_point(color = color_points,
                         fill  = color_points,
                         alpha = alpha_points,
                         shape = 21,
                         size = 2) +
    ggplot2::geom_boxplot(data = maxN_plot_df[which(maxN_plot_df$graph_type == "boxplot"), ],
                          ggplot2::aes(x = maxN_plot_df$maxN_index[which(maxN_plot_df$graph_type == "boxplot")],
                                       y = maxN_plot_df$values[which(maxN_plot_df$graph_type == "boxplot")]),
                          color = color_boxplots,
                          fill  = color_boxplots,
                          alpha = alpha_boxplots) +
    ggplot2::ylab("maxN values") +
    ggplot2::xlab("index")


  # return the plot:
  return(maxN_plot)
}
