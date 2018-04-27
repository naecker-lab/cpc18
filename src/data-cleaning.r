# data-cleaning.r
# Importing and cleaning data for CPC18 project

#### PACKAGES #####
library(dplyr)
library(readr)
library(purrr)

#### FUNCTIONS ####

# calculate_prob
# Function to a particular binomial probability
# inputs: number of outcomes LotNum, position k
# output: a probability
calculate_prob <- function(LotNum, k){
  choose(LotNum, k)*0.5^LotNum
}

calculate_prob(3, 1)

# expand_lottery
# Function to convert from compact lottery info to full payoffs and probabilities of the lottery
# inputs: high payoff H, probabiliy of high payoff pH, low payoff L, lottery shape LotShape, number of outcomes LotNum
# output: a list of full payoffs and probabilities for the lottery
expand_lottery <- function(H, pH, L, LotShape, LotNum){
  if (LotShape == "Symm") {
    outcomes <-  seq(from = H - LotNum/2, to = H + LotNum/2, by = 1)
    probs <- map_dbl(c(0:LotNum), ~calculate_prob(LotNum, .))
  } else if(LotShape == "R-skew"){
    outcomes <- c(0:LotNum)
    probs <- c(0:LotNum)
  } else if(LotShape == "L-skew"){
    outcomes <-  c(0:LotNum)
    probs <- c(0:LotNum)
  }
  
  list(outcomes = outcomes, probs = probs)
}

expand_lottery(100, 0.5, 0, "Symm", 9)

  #### SCRIPT #####
# note: all paths should be relative to root of cpc18 repo
df <- read_csv("data/raw-data.csv")

# make participants data set
participants <- 
  df %>%
  select(SubjID, Location, Gender, Age, Set, Condition) %>%
  unique()

# make games data set
games <- 
  df %>%
  select(GameID, Ha, pHa, La, LotShapeA, LotNumA, Hb, pHb, Lb, LotShapeB, LotNumB, Amb, Corr) %>%
  unique() %>%
  mutate(EVA = pHa * Ha + (1-pHa)*La,
         EVB = pHb * Hb + (1-pHb)*Lb)

# make decisions data set
decisions <- 
  df %>%
  select(SubjID, GameID, Order, Trial, Button, B, Payoff, Forgone, RT, Apay, Bpay, Feedback, block)

# save everything as data frames in a single database file
save(df, participants, games, decisions, file = "data/clean-data.Rdata")