montyHall <- function(nTrials, switch) {
  win <- NULL # Create vector of wins
  for (i in seq_len(nTrials)) { # Run nTrials
    car <- floor(runif(1, 1, 4)) # Choose a random door with a car behind it
    chosen <- floor(runif(1, 1, 4)) # Choose a random "guessing" door
    # Enumerate all the possible cases for strategies and chosen doors
    if (switch == TRUE && car == chosen) {
      win[i] <- 0 
    } else if (switch == TRUE && car != chosen) { 
      win[i] <- 1
    } else if (switch == FALSE && car == chosen) {
      win[i] <- 1
    } else if (switch == FALSE && car != chosen) {
      win[i] <- 0
    }
  }
  # Get the winning probability confidence intervals
  ci <- prop.test(sum(win), nTrials)$conf.int
  if (switch == TRUE) {
    cat("Winning Probability for Switching: ", mean(win), "\n", sep = "")
  } else {
    cat("Winning Probability for NOT Switching: ", mean(win), "\n", sep = "")
  }
  cat("95% CI: [", ci[1], ", ", ci[2], "]", sep = "")
}

# Test the 2 strategies
montyHall(100000, TRUE)
montyHall(100000, FALSE)

# The winning probability for switching doors after a door is opened is about
# 66% with a confidence interval of approximately [0.65 - 0.70]. The winning 
# probability for not switching doors after a door is opened is about 33% with
# a confidence interval of approximately [0.33 - 0.34]. Intuitively, this makes
# sense because one would initially pick a door with a goat behind it 
# approximately 2/3 times. This means that 2/3 times, the host will be forced to
# open the only other door with a goat behind it and thus, the final door will 
# have a car behind it 2/3 times. Therefore, to have a 2/3 chance of winning,
# one should switch doors. Only on 1/3 tries, will one be switching to a door
# with a goat behind it since there is only a 1/3 initial chance that the user
# picks a car at first.

