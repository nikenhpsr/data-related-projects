#Installing packages
install.packages("ggplot2")
install.packages("tidyverse")
devtools::install_github("kassmbara/datarium")

#Load the packages
library(ggplot2)
library(tidyverse)

# Load the data
data("marketing", package = "datarium")
head(marketing, 4)

#Scatter plot visualization
ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth()

#Displaying correlation between youtube advert and sales
cor(marketing$sales, marketing$youtube)

#Modeling simple linear regression
model <- lm(sales ~ youtube, data = marketing)
model

#Ploting regression line
ggplot(marketing, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm)

#Summary
summary(model)
