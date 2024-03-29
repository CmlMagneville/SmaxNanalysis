% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/9_Stats_functions.R
\name{glmm.compute}
\alias{glmm.compute}
\title{Compute a GLMM and test it}
\usage{
glmm.compute(
  SmaxN_df,
  Y_var,
  X_var,
  X_var_random,
  family_law,
  check_resid,
  compute_RNakag
)
}
\arguments{
\item{SmaxN_df}{a dataframe with the different variables to use in the model}

\item{Y_var}{the name of the answer variable (name must be the same of the
related SmaxN_df column)}

\item{X_var}{a vector gathering the name of the effect variable(s) (name(s)
must be the same of the related SmaxN_df column). Just one or two X var allowed.}

\item{X_var_random}{a vector gathering the name of the random effect variable(s)
(name(s) must be the same of the related SmaxN_df column). If no, NA. BUT
the code does not use the names in the vector and uses species_nm if one
random effect and species_nm and pose_nb if two random effects.}

\item{family_law}{the family of the glmm}

\item{check_resid}{a boolean value (TRUE/FALSE) telling whether the residuals of the
model are checked or not}

\item{compute_RNakag}{a boolean value (TRUE/FALSE) telling whether the Nakagawa's R2
is computing or not}
}
\value{
a plot of the repartition of
}
\description{
This function computes a GLMM after studying the Y distribution, give the
related ANOVA table and test it
}
