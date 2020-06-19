#### pedroso_gen.R ####

# This file defines functions used to implement the Pedroso et. al. (2018) method of synthetic data generation.
# The general workflow proceeds in roughly five steps:
#     1 - Define an overall index, either algorithmically or from another source
#     2 - Define a set of trend boundaries, either algorithmically or from another source
#     3 - Split the collection of assets into time periods based on the trend boundaries
#     4 - Fit some representation of the movements within each trend period
#     5 - Generate data from these representations in a desirable sequence
#
# The original paper suggests several nonoverlapping windows of multivariate normals as the representation
# and suggests alternating up and down trends in the generation process, which corresponds to their
# chosen trend boundary method which alternates up and down.

#' Detect Trend Change Points
#' 
#' @export

trend_DCC <- function(price_ts){
  dc <- ext <- 1
  for(i in 1:length(price)){
    if(length(dc) %% 2 == 1){ #down trend
      ext <- which.min(price[ext:i]) + ext - 1
    } else { #up trend
      ext <- which.max(price[ext:i]) + ext - 1
    }
    
    if(abs((price[i] - price[ext])/price[ext]) > theta){
      dc <- c(dc, ext)
    }
  }
  if(dc[2]-dc[1] < 2) dc <- dc[-2] #Trend minimum here
  return(c(dc,length(price)))
}

#' Generator Construction
#' 
#' Construct object from which generations can be drawn
#' 
#' @export

construct_generator <- function(Rmat, trend_inds){
  
}



