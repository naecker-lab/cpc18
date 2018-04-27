# functions.r
# Custom functions for CPC18 project

# calculate_prob
# Function to a particular binomial probability
# inputs: number of outcomes LotNum, position n
# output: a probability
calculate_prob <- function(k, n){
  choose(k, n)*0.5^k
}

# expand_lottery
# Function to convert from compact lottery info to full payoffs and probabilities of the lottery
# inputs: high payoff H, probabiliy of high payoff pH, low payoff L, lottery shape LotShape, number of outcomes LotNum
# output: a list of full payoffs and probabilities for the lottery
expand_lottery <- function(H, pH, L, LotShape, LotNum){
  
  # notation to match definition of lotteries in paper
  k = LotNum - 1
  n = LotNum
  
  # expand H payoff
  # symmetric lotteries
  if (LotShape == "Symm") {
    outcomes <-  seq(from = H - k/2, to = H + k/2, by = 1)
    probs <- map_dbl(c(0:k), ~calculate_prob(k, .))
    
    # Right skewed lotteries
  } else if (LotShape == "R-skew") {
    outcomes <- map_dbl(c(1:n), ~2^.-n-1+H)
    probs <- map_dbl(c(1:n), ~0.5^.)
    probs[n] <- 1 - sum(probs[1:n-1])
    
    # Left skewed lotteries  
  } else if (LotShape == "L-skew") {
    outcomes <- map_dbl(c(1:n), ~n+1-2^.+H)
    probs <- map_dbl(c(1:n), ~0.5^.)
    probs[n] <- 1 - sum(probs[1:n-1])
  }
  
  # expand lotery num 1 cases
  else if (LotShape == "-") {
    outcomes <- c(H)
    probs <- c(pH)
  }
  
  # compound in L outcome
  outcomes <- c(outcomes, L)
  probs <- c(pH*probs, 1 - pH)
  EV = sum(probs*outcomes)
  
  # output as list since number of outcomes/probs changes
  list(outcomes = outcomes, probs = probs, EV = EV)
}

# vectorize this function
expand_lottery_v <- Vectorize(expand_lottery)