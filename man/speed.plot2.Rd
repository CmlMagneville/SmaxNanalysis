% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/8_SmaxN_fish_speed_function.R
\name{speed.plot2}
\alias{speed.plot2}
\title{Create and save the plot of SmaxN ratio for two different fish speeds and a set
of species}
\usage{
speed.plot2(maxN_speed1, maxN_speed2, color_poses, alpha, size, shape)
}
\arguments{
\item{maxN_speed1}{a dataframe from the automat.maxN.setsp() function
gathering maxN values for each species of the set and for each pose computed
with the first speed}

\item{maxN_speed2}{a dataframe from the automat.maxN.setsp() function
gathering maxN values for each species of the set and for each pose computed
with the second speed}

\item{color_poses}{a vector containing the names or hexadecimal code of the three
colors used to plot the different poses}

\item{alpha}{a vector containing the values of the two
transparencies used to plot the different speed}

\item{size}{a numerical value referring to the size of the points used to
plot SmaxN}

\item{shape}{a numerical value referring to the shape of the points used to
plot SmaxN for each pose}
}
\value{
a plot with y = SmaxN values with two different colors for the two
different speeds, x = species
}
\description{
This function computes and save in the outputs folder a plot showing the
SmaxN ratio between two speeds (y) for a set of species x
}
