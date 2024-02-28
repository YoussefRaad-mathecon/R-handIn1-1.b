Bach <- function(spot, strike, timetomat, t, sigma){
  X = (spot - strike) / (sigma * sqrt(timetomat - t))
  delta_bach <- pnorm(X) 
  prices_bach <- (spot - strike) * pnorm(X) + sigma * sqrt(timetomat - t) * dnorm(X)
  return(prices_bach)
}

BS <- function(spot, strike, t, r, div, timetomat, sigma){
  d1 = (1 / (sigma * sqrt(timetomat - t))) * (log(spot / strike) + (r + 0.5 * sigma ^ 2) * (timetomat - t))
  d2 = d1 - sigma * sqrt(timetomat - t)
  BS_prices = spot * exp(-div * timetomat) * pnorm(d1) - exp(-r * (timetomat - t)) * strike * pnorm(d2)
  return(BS_prices)
}

IV <- function(spot, strike, timetomat, sigma){
  difference <- function(sigBS){
    return(Bach(spot, strike, timetomat, 0, sigma) - BS(spot, strike, 0, 0, 0, timetomat, sigBS))
  }
  return(uniroot(difference, lower = 10^-6, upper = 10)$root)
}


S0 = 100
T = 1
sigma = 15


K <- seq(50, 150, 0.1) 
ImpVol = rep(NA, length(K))

for(i in 1:length(K)){
  ImpVol[i] = IV(S0, K[i], T, sigma)
}

plot(K, ImpVol, type = "l", col="red",
     xlab = "Strikes",
     ylab = expression("Implied Volatilities"),
     main = "Implied Volatilities vs. Strikes")
points(100, ImpVol[which.min(abs(K-100))], col="blue", pch=19)

