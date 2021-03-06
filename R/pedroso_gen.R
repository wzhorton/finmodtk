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

trend_DCC <- function(price_ts, theta = 0.5, min_len = 2){
  dc <- ext <- 1
  for(i in 1:length(price_ts)){
    if(length(dc) %% 2 == 1){ #down trend
      ext <- which.min(price_ts[ext:i]) + ext - 1
    } else { #up trend
      ext <- which.max(price_ts[ext:i]) + ext - 1
    }
    
    if(abs((price_ts[i] - price_ts[ext])/price_ts[ext]) > theta){
      dc <- c(dc, ext)
    }
  }
  dc <- c(dc,length(price_ts))
  while(dc[2]-dc[1] <= min_len) dc <- dc[-2] 
  return(dc)
}

#' Generator Construction
#' 
#' Construct object from which generations can be drawn
#' 
#' @export

construct_generator <- function(return_mat, trend_inds, win_len = 4){
  trend_periods <- lapply(1:(length(trend_inds)-1), function(i){
    return_mat[,trend_inds[i]:(trend_inds[i+1]-1), drop = FALSE]
  })
  
  lapply(trend_periods, function(rmat){
    nwins <- max(floor(ncol(rmat)/win_len),1)
    if(nwins > 1){
      win_id <- as.numeric(cut(1:ncol(rmat), nwins))
    } else {
      win_id <- rep(1, ncol(rmat))
    }
    win_count <- table(win_id)
    lapply(1:length(win_count), function(i){
      id <- as.numeric(names(win_count)[i])
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
    blockwin <- lapply(go, function(gowin){
      t(MASS::mvrnorm(gowin$npts, gowin$mu, gowin$sig))
    })
    do.call(cbind, blockwin)
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
#' Resulting prices are normalized to begin at one.
#' 
#' @export

pedroso_synthesis <- function(prices, ntrend, theta = 1, extra = 0, repl = FALSE, win_len = 4){
  index_price <- colMeans(prices)
  trend_bounds <- trend_DCC(index_price, theta)
  returns <- t(apply(prices, 1, as_returns))
  fitted_plbb <- construct_generator(returns, trend_bounds, win_len)
  synth_returns <- synthesis(fitted_plbb, ntrend, extra, repl)
  synth_prices <- t(apply(synth_returns, 1, as_prices))
  return(list(synth_prices = synth_prices, synth_returns = synth_returns))
}


