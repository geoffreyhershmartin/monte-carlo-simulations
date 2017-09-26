middleSquare <- function(nRand, nDigits, seed) {
  seed <- seed * 10^nDigits
  vector <- rep(0, nRand)
  for (i in 1:nRand) {
    vector[i] <- (floor((seed^2) %/% (10^(nDigits/2))) %% (10^nDigits))
  }
  return(vector)
}