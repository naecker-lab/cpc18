# Data_Exploration.r
# Script for general exploratory data analysis of CPC18 data

#### PACKAGES ####
library(Hmisc)
library(dplyr)
library(ggplot2)
library(data.table)

##### SCRIPT #####
# load data
# note: all paths should be relative to root of cpc18 repo
load("data/clean.Rdata")

# look at age distributions
ggplot(data=participants, aes(Age))+
  geom_bar(aes(y= 100*(..count..)/sum(..count..))) +
  labs(x="Age", y="Percent") + 
  facet_grid(Location ~ Gender + Condition)

# do people tend to choose higher expected value option?
decisions %>%
  merge(games) %>%
  xtabs(~ B + B_is_better, data = .)

# is probability of choosing option B proportional to how much more expected payoff B gives?
decisions %>%
  merge(games) %>%
  lm(B ~ EV_diff, data = .) %>%
  summary()

# same as above, but graphically (for one subject since plotting everything is very slow)
decisions %>%
  filter(SubjID == 10103) %>%
  merge(games) %>%
  ggplot(aes(x=EV_diff, y=B)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.1) + 
  stat_smooth(method = "lm")

# counting frequency of changes for each game-subject combination
decisions %>% 
  group_by(GameID, SubjID) %>% 
  summarise(changes=sum(Change, na.rm=T)) 

## by ID and by game, percent of wins/ losses
decisions %>%
  group_by(SubjID, GameID) %>%
  summarise(
    win_percent = mean(Win),
    change_percent = mean(Change, na.rm = T)
  )

# plot decision over time for one person
decisions %>%
  filter(SubjID == 10102) %>%
  ggplot() +
    geom_point(aes(x= Trial, y = Button, color = as.factor(Win)))+
    geom_vline(xintercept=5, linetype = 2, color = "red") +
    facet_wrap(~GameID)
