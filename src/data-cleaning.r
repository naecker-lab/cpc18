# data-cleaning.r
# Importing and cleaning data for CPC18 project

#### PACKAGES #####
library(dplyr)
library(readr)
library(purrr)
library(Hmisc)
library(data.table)


#### FUNCTIONS ####
source("src/functions.R")

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
         EVB = pHb * Hb + (1-pHb)*Lb, 
         B_is_better = EVB >= EVA, 
         EV_diff = EVB - EVA
         )

 # make decisions data set
decisions <- 
  df %>%
  select(SubjID, GameID, Order, Trial, Button, B, Payoff, Forgone, RT, Apay, Bpay, Feedback, block) %>%
  mutate(
    WinMagnitude = Payoff - Forgone,
    Win = WinMagnitude > 0,
  ) %>%
  group_by(SubjID, GameID) %>%
  mutate(
    LagB = shift(B, 1),
    Change = B != LagB
  ) %>%
  ungroup()

# save everything as data frames in a single database file
save(df, participants, games, decisions, file = "data/clean.Rdata")