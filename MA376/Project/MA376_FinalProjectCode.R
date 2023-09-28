##Libraries Required to Run Code
library(Lahman)
library(tidyverse)
library(table1)
library(knitr)
library(ggResidpanel)


##Merge Lahman Data Frames to Obtain Player Name and Batting Hand
Master$name <- paste(Master$nameFirst, Master$nameLast, sep=" ")
batting <- merge(Batting,
                 Master[,c("playerID","name","bats")],
                 by="playerID", all.x=TRUE)


##Filter Batting Data Based on Inclusion Criteria
battingData <- batting %>%
  filter(yearID == 2019) %>%
  filter(AB >= 100) %>%
  filter(bats == "R" | bats == "L") %>%
  mutate(strikeoutRate = SO/AB,
         homerunRate = HR/AB,
         battingAverage = H/AB) %>%
  select(name,bats,strikeoutRate,homerunRate,battingAverage)


##Table 1 Output
table1(~strikeoutRate + homerunRate + battingAverage | as.factor(bats), data = battingData)


##Plot Distribution of Strike Out Rates
battingData %>%
  ggplot(aes(x= strikeoutRate))+
  geom_histogram()+ 
  labs(x = "Strike Out Rate") +
  ggtitle("Distribution of Strike Out Rates")

##Plot Relationship Between Strike Outs and Home Runs (By Batting Hand)
battingData %>%
  ggplot(aes(y= strikeoutRate, x= homerunRate, color= bats))+
  geom_point()+ 
  geom_smooth(method = "lm") + 
  labs(y = "Strike Out Rate", x = "Home Run Rate", color = "Bats") +
  ggtitle("Relationship Between Strike Outs and Home Runs (By Batting Hand)")


##Plot Relationship Between Strike Outs and Hits (By Batting Hand)
battingData %>%
  ggplot(aes(x= battingAverage, y= strikeoutRate, color= bats))+
  geom_point()+ 
  geom_smooth(method = "lm") + 
  labs(x = "Batting Average", y = "Strike Out Rate", color = "Bats") +
  ggtitle("Relationship Between Strike Outs and Hits (By Batting Hand)")

##Building a Linear Model
model1 <- lm(strikeoutRate ~ homerunRate + battingAverage + as.factor(bats), data = battingData)
summary(model1)

##ANOVA Table
anova(model1)


##Checking Validity Conditions
resid_panel(model1, plots = c('hist', 'resid'), bins = 17)
