# Knuth's Super Random Number Generator

library(gmp)

superRandom <- function(nRand, seed) {
  randomVector <- rep(as.bigz(0), 1)
  for (i in 1 : nRand)
  {
    seed <- as.bigz(format(seed, digits = "20"))
    y <-  divq.bigz(seed, 10 ^ 9) 
    seed <- steps[[1]](seed, y)
    randomVector[i] <- seed
  }
  return(randomVector)
}

steps <- list(
  K2 <- function(seed, y) {
    z <- mod.bigz(divq.bigz(seed, (10^8)), 10)
    z <- floor(z)
    steps[[2 + as.numeric(z)]](seed, y)
  },
  
  K3 <- function(seed, y) {
    if (seed < (5 * 10 ^ 9)) {
      seed <- add.bigz(seed, (5 * 10 ^ 9))
    }
    steps[[3]](seed, y)
  },
  
  K4 <- function(seed, y) {
    seed <- mod.bigz(divq.bigz(pow.bigz(seed, 2), (10 ^ 5)), (10 ^ 10))
    steps[[4]](seed, y)
  },
  
  K5 <- function(seed, y) {
    seed <- as.bigz(mul.bigz(1001001001, seed), (10 ^ 10))
    steps[[5]](seed, y)
  },
  
  K6 <- function(seed, y) {
    if (seed < (10 ^ 8)) {
      seed <- add.bigq(seed, 9814055677)
    } else {
      seed <- sub.bigz((10 ^ 10), seed)
    }
    steps[[6]](seed, y)
  },
  
  K7 <- function(seed, y) {
    seed <- as.bigz(seed)
    seed <- add.bigz(mul.bigz((10 ^ 5), (mod.bigz(seed, (10 ^ 5)))), divq.bigz(seed, (10 ^ 5)))
    steps[[7]](seed, y)
  },
  
  K8 <- function(seed, y) {
    seed <- mod.bigz(mul.bigz(1001001001, seed), (10 ^ 10))
    steps[[8]](seed, y)
  },
  
  K9 <- function(seed, y) {
    seed <- as.character(seed)
    seed <- strsplit(as.character(seed), "")[[1]]
    new_seed <- rep("", 10)
    for (number in 1 : length(seed)) {
      if (seed[number] != "0") {
        new_seed[number] <- as.character(as.bigz(seed[number]) - 1)
      } else {
        new_seed[number] <- "0"
      }
    }
    new_seed <-  gsub(", ", "", toString(new_seed))
    seed <- as.bigz(as.numeric(new_seed))
    steps[[9]](seed, y)
  },
  
  K10 <- function(seed, y) {
    if (seed < as.bigz(100000)) {
      seed <- pow.bigz(seed, 2) + 99999
    } else {
      seed <-  seed - 99999
    }
    steps[[10]](seed, y)
  },
  
  K11 <- function(seed, y) {
    if (seed < as.bigz(10 ^ 9)) {
      seed <- 10 * seed
      steps[[10]](seed, y)
    }
    steps[[11]](seed, y)
  },
  
  K12 <- function(seed, y) {
    seed <- mod.bigz(divq.bigz((mul.bigz(seed, (seed - 1))), (10 ^ 5)), (10 ^ 10))
    steps[[12]](seed, y)
  },
  
  K13 <- function(seed, y) {
    if (y > 0) {
      y <- y - 1
      steps[[1]](seed, y)
    } else {
      return(seed)
    }
  }
)