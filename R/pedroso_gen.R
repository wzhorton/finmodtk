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

construct_generator <- function(return_mat, trend_inds, win_len = 4){
  trend_periods <- lapply(1:(length(trend_inds)-1), function(i){
    return_mat[,dc[i]:dc[i+1]]
  })
  
  lapply(trend_periods, function(rmat){
    win_id <- as.numeric(cut(1:11, floor(ncol(rmat/win_len))))
    win_count <- table(win_id)
    lapply(1:length(win_count), function(i){
      id <- names(win_count)[i]
      rdat <- t(rmat[,win_id == id])
      mu = colMeans(rdat)
      sig = cov(rdat)
      list(npts = win_count[i], mu = mu, sig = sig)
    })
  })
}

#' Synthesize Data
#' 
#' Generate/synthesize data using a generation object. This method might be
#' described as a parametric local blocked bootstrap. Default behavior is to
#' randomly generate a specified number of trend sequences, however the replication
#' option can be specified to exactly match historical records. PCA recomposition
#' is also used to generate a number of new, extra, unseen assets.
#' 
#' @export

synthesis <- function(gen_obj, ntrend, extra = 0, repl = FALSE){
  trend_seq <- 1:length(gen_obj)
  if(repl == FALSE) {
    evens <- trend_seq[trend_seq %% 2 == 0]
    odds <- trend_seq[trend_seq %% 2 == 1]
    trend_seq <- c(rbind(sample(evens, ceiling(ntrend/2)+1, replace = TRUE), 
      sample(odds, ceiling(ntrend/2)+1, replace = TRUE)))
    if(runif(1)>0.5) trend_seq <- trend_seq[-1]
    trend_seq <- trend_seq[1:ntrend]
  }
  
  return_blocks <- lapply(trend_seq, function(tr){
    go <- gen_obj[[tr]]
    t(MASS::mvrnorm(go$npts, go$mu, go$sig))
  })
  return_out <- do.call(cbind, return_blocks)
  
  if(extra >= 1){
    pca_returns <- prcomp(t(return_out), center = FALSE)
    mn_pca <- rowMeans(pca_returns$rotation)
    vr_pca <- cov(t(pca_returns$rotation))
    new_rot <- t(MASS::mvrnorm(extra, mn_pca, vr_pca))
    aug_rot <- cbind(pca_returns$scores, new_rot)
    return_out <- t(pca_returns$x%*%t(aug_rot))
  }
  
  return_out
}

#' Pedroso Data Generation
#'
#' Generates data using a given price dataset using the Pedroso et. al. (2018) method.
#' Mo

#pedroso_synthesis 


