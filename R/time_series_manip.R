#### time_series_manip.R ####

#' Convert Time Series Prices to Returns
#' 
#' Converts a sequential series of prices to a series of returns.
#' 
#' @param prices Numeric vector of returns.
#' @export

as_returns <- function(prices){
  dprices <- diff(prices)
  dprices/tail(prices,-1)
}

#' Convert Time Series Returns to Prices
#' 
#' Converts a sequential series of returns to a series of prices which originate
#' at some initial value.
#' 
#' @param returns Numeric vector of returns.
#' @param p0 Numeric; initial price value.
#' @export

as_prices <- function(returns, p0 = 1){
  c(1,cumprod(returns+1))*p0
}

#' Normalize Prices
#' 
#' Normalizes price time series such that the initial price is 1 unit.
#' 
#' @export

price_norm <- function(prices){
  as_prices(as_returns(prices))
}