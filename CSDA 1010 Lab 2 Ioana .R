##Loading the dataset

setwd("~/Downloads/")
housedata<-read.csv("kc_house_data.csv")

head(housedata)
str(housedata)

colnames(housedata)
summary(housedata)
library("psych")
describe(housedata)

##clean date
housedata[,'date'] <- as.Date(housedata[,'date'], format="%Y%m%dT000000")
cat("Number of unconverted dates:", sum(is.na(housedata$date))) 

##check high level relationships
plot(housedata[,c("price", "date", "bedrooms", "bathrooms", "sqft_living", "sqft_lot", "floors", "waterfront", "view")])
plot(housedata[,c("sqft_living", "sqft_lot", "sqft_above", "sqft_basement", "sqft_living15", "sqft_lot15")])

#Relationships potentially identified between sqft_living and sqft_above, sqft_basement, and sqft_living15. 

cor_housedata <- cor(housedata[,c("price", "bedrooms", "bathrooms", "sqft_living", "sqft_lot", "yr_built", "yr_renovated", "sqft_above", "sqft_basement", "sqft_living15", "sqft_lot15")])
install.packages("corrplot")
library("corrplot")
corrplot(cor_housedata, order = "hclust")
#Sqft_living appears to have the strongest relationship to price. Recommend discarding  sqft_above, sqft_basement, and sqft_living15


##Showing mean house price over time
library(dplyr)
library(ggplot2)
library(lubridate)
plot(housedata$date, housedata$price, type="l", col="red")

housedata %>%
  mutate(date = ymd(date)) %>%
  ggplot(aes(x = date, y = price)) +
  geom_point() +
  # Calculate the mean based on y, set geom = line
  stat_summary(fun.y = "mean", colour = "red", size = 2, geom = "line")
