#importing dataset
library(readxl)
data_frame <- read_excel("C:/Users/USER/Desktop/KEMRI/RStudio/height.xls")
library(tidyverse)
library(dplyr)

#cleaning dad_hand
data_frame$dad_hand[data_frame$dad_hand == 'l'] <- 'L'
data_frame$dad_hand[data_frame$dad_hand == 'r'] <- 'R' 
#cleaning mum_hand
data_frame$mom_hand[data_frame$mom_hand == 'l'] <- 'L'
data_frame$mom_hand[data_frame$mom_hand == 'r'] <- 'R'

#convert weight variable from char to num
data_frame$weight <- as.numeric(data_frame$weight)
str(data_frame$weight)

#missing values in weight
sum(is.na(data_frame$weight))

#cleaning weight
data_frame$weight[is.na(data_frame$weight)] <- 0

#cleaning dad_height and mom_height, sex
data_frame$dad_height[data_frame$dad_height <= 69] <- 70
data_frame$mom_height[data_frame$mom_height <= 60] <- 70

#creating new variable BMI
weightKG = data_frame$weight*0.4536
heightM = data_frame$height*0.0254
data_frame <- data_frame %>% mutate(BMI = (weightKG/(heightM*heightM))) 

#average & median BMI val
summary(data_frame$BMI)

# creating new variable BMIcat
data_frame$BMIcat <- ifelse(data_frame$BMI <= 18.4, 'under', 
                            ifelse(data_frame$BMI >= 18.5 & data_frame$BMI < 24.9, 'normal', 'over'))

#cleaning the sex variable
data_frame$sex[data_frame$sex  != 'F'] <- 'M'
data_frame$sex[data_frame$sex <= 'f'] <- 'F'

#creating new variable gender
data_frame$gender <- ifelse(data_frame$sex == 'M', 'Male', 'Female')

#piechart, scatter, box and bar plots
x <- c(length(which(data_frame$hand == 'R')),length(which(data_frame$hand == 'L')))
y <- c('R','L')
pie(x,y)
#scatter
scatter.smooth(data_frame$dad_height, data_frame$mom_height)
#box
b <- table(data_frame$weight, data_frame$hand)
boxplot(b)
#bar
br <- table(data_frame$BMIcat)
barplot(br)

#underweight persons, overweight men, right handed women over 30yrs with normal weight
sum(data_frame$BMIcat == 'under')
#overweight men
sum(data_frame$sex == 'M' & data_frame$BMIcat == 'over')
#right handed women over 30yrs with normal weight
w = sum(data_frame$hand == 'R' & data_frame$sex == 'F' & data_frame$age >= 30 & data_frame$BMIcat == 'normal')
print(w)
#percentage
perc = (w/70)*100
print(perc)

#display cleaned dataset
View(data_frame)

#Josh R link for practice work
data_frame %>% filter(gender == 'Male' & age > 30 & hand == 'L')
data_frame <- data_frame %>%
  arrange(id)
view(data_frame)

#data_frame <- data_frame %>%
  mutate(height = case_when(
    height == 69 ~ "6ix9ine",
    height == 70 ~ "Seventy"
  ))
View(data_frame)










