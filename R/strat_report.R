#### strat_report.R ####

#' Generate Strategy Report
#'
#' Evaluates a strategy on several metrics and aggregates the results into a list
#'
#' @export

strat_report <- function(prices, strats, equity_plot = FALSE){
  r <- as_returns(prices)
  sr <- strat_returns(prices, strats)
  e <- equity_curve(prices, strats)
  
  out <- list()
  
  if(equity_plot){
    plot(e, type = "l")
  }
  
  out$mean_return <- mean(sr)
  out$total_return <- tail(e,1)
  out$mean_positive_return <- mean(sr[sr>0])
  out$mean_negative_return <- mean(sr[sr<0])
  out$win_loss_ratio <- sum(sr>0)/sum(sr<0)
  out$positive_error_rate <- sum(r>0 & sr<0)/sum(r>0)
  out$negative_error_rate <- sum(r<0 & sr<0)/sum(r<0) 
  out$positive_win_rate <- sum(r>0 & sr>0)/sum(r>0)
  out$negative_win_rate <- sum(r<0 & sr>0)/sum(r<0)
  out$sd <- sd(sr) #we should consider non-zero volatility as well
  out$VaR <- quantile(sr, 0.05)
  out$TVaR <- mean(sr[sr<=out$VaR])
  return(out)
}