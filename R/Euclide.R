## GPL-3 License
## Copyright (c) 2021 Vrunge

#' Computing euclidian distance
#'
#' @description  Euclidian Distance with R functions
#' @param v a vector of numeric data
#' @param w a vector of numeric data
#' @return the euclidian distance between v and w
Euclide_R <- function(v,w)
{
  return(sqrt(sum((v-w)^2)))
}

#' Computing euclidian distance
#'
#' @description  Euclidian Distance with R functions and a for loop
#' @param v a vector of numeric data
#' @param w a vector of numeric data
#' @return the euclidian distance between v and w
Euclide_R_for <- function(v,w)
{
  n <- length(v)
  temp <- 0
  for(i in 1:n)
  {
    temp <- temp + (v[i]-w[i])^2
  }
  return(sqrt(temp))
}

