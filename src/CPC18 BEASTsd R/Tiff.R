raw_data <- read_csv("E:/Senior/RA/cpc/cpc18/data/raw-data.csv")

summary(raw_data)
length(unique(as.factor(raw_data$GameID)))
