#Data Exploration 
library(Hmisc)
library(dplyr)
library(ggplot2)
library(data.table)

##import the dataset
df <- read.csv("~/Desktop/repos/cpc18/data/raw-data.csv")


##general descriptives of the whole dataset
x<- describe(df)
x

##create a counter by group... would need to sort by game first
# df2<- df %>%
#   group_by(SubjID) %>%
#   sort(GameID) %>%
#   mutate(round_num = row_number())

##Subset data
df2<- df[, c(1, 7, 21, 8:10, 13:15, 22:25, 29)]
df2 <- df2[order(df2$SubjID, df2$GameID, df2$Trial),]

#make the button they selected into a factor
df2$Button <- as.factor(df$Button)
df2$GameID <- as.factor(df2$GameID)


df2$win_mag <- df2$Payoff - df2$Forgone
df2$win <- ifelse(df2$Payoff - df2$Forgone > 0, 1, 0)

##did their decison change
##take previous choice, see if it is different from current choice
df3 <- df2 %>%
  group_by(SubjID, GameID)%>%
  mutate(lag = shift(B, 1),
         change = ifelse(B == lag, 0 ,1))



##by ID, percent of wins/ losses
win <- df3 %>%
  group_by(SubjID, GameID) %>%
  summarise(win_percent = mean(win),
            change_percent = mean(change, na.rm = T))
win_stats <- win %>%
  group_by(SubjID) %>%
  summarise(total_win_percent = mean(win_percent))

##dataset for just person 1
p1 <- filter(df3, SubjID == 10102)

View(filter(p1, Trial <= 5))

#plot decision over time, no feedback
ggplot(data = filter(p1, Trial <= 5)) +
  geom_point(aes(x= Trial, y = Button, color = as.factor(win))) +
  facet_wrap(~GameID)

#plot all decisions over time
ggplot(data = p1) +
  geom_point(aes(x= Trial, y = Button, color = as.factor(win)))+
  geom_vline(xintercept=5, linetype = 2, color = "red") +
  facet_wrap(~GameID)
