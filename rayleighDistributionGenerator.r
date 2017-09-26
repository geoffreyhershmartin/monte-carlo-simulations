# Rayleigh Distribution Generator
rrayleigh <- function(n,sigma){ 
  ssq <- 2 * (sigma ^ 2)  # squaring sigma
  y <- runif(n) # generating n numbers of uniform random numbers 
  rnd <- sqrt(-ssq*log(y)) # assigning  Rayleigh distributed random numbers to a vector
}
random <- rrayleigh(1e5, 1)  # generate 10,000 Rayleigh distributed numbers. 

# 2c
rayleigh <- function(x){ # creating the function that describe CDF of Rayleigh distribution. 
  1 - exp((-x^2) / 2)
}

cat("p-value returned from KS test:", ks.test(random, "rayleigh")$p.value)  # performing ks test 

