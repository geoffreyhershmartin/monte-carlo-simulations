# Buffon's Needle Simmulation

# Estimate pi from crossing probability p. L is the needle length,
# d the distance between strips on the floor equals 1.
piFromProb <- function(p, L, d) {         
  if (L <= d) 
    return(2 * L / (p * d))
  return(2 * (asin(d/L) - L/d + sqrt(L^2/d^2 - 1)) / (1-p))
}

# Monte Carlo simulation of Buffon's needle.
buffon <- function(throws, L, d) {
  x <- runif(throws, 0, d/2)          # Random numbers in (0, d/2).
  theta <- runif(throws, 0, 0.5*pi)   # Random numbers in (0, pi/2).
  hits <- (x <= 0.5*L*sin(theta))     # hits is TRUE if needle crosses
  pcross <- mean(hits)                # a strip.
  piEst <- piFromProb(pcross, L, d)
  ci <- prop.test(sum(hits), throws)$conf.int
  ci <-                         # Apply transformation to get CI.
    sort(piFromProb(ci, L, d))  # sort() is necessary because, if L<=d,
  # the upper CI boundary for pcross
  # gives rise to the lower CI boundary
  # for piEst.
  return(ci[2] - ci[1])
}

# Create a vector of ratios
ratios <- seq(0.1, 5, 0.1)
intervals <- NULL
# Get all the interval widths
for (i in 1 : length(ratios)) {
  intervals[i] <- buffon(100000, ratios[i], 1)
}

# Plot all the results
plot(ratios, intervals)
cat(ratios[which.min(intervals)])

# To choose an opitmum L/d, should choose the ratio of L/d to be 1, i.e., L = d. 
# From the plot of interval width as a function of ratios, it would seem that the interval
# width decreases as the ratio of L/d converges to 1. Then, the interval width
# increases linearly as the ratio of L/d increases from 1. This would indicate
# that as the length of the needle grows to d, the variance of the
# hits grows smaller. After L/d > 1, we expect the probability that a
# needle crosses a line to increase on every try; now a needle can cross
# 2 lines on one drop and variance increases. Mathematically, since pcross = 
# E(X) = 2L / pi (since we let d = 1 here), and var = E(X^2) - (E(X))^2, this
# follows as var decreases as L/d, i.e., L, increases below 1 while var 
# increases as L/d increases above 1. Thus, the optimum L/d should be 1.