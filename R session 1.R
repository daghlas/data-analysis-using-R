getwd()

#setting working directory
#setwd("C:/Users/Ivlyn/Desktop/Eddy/R sessions")

#loading packages
install.packages()

#installing packages
library(magrittr)
library(tidyverse)
library(readxl)

#importing data to R
height <- read_excel("C:/Users/USER/Desktop/Colaboratory/height.xls")
View(height)

#No of variables
dim(height)

#No of observation
nrow(height)

#type of variable hand
str(height$hand)
glimpse(height$hand)

#type of variable mom_height
str(height$mom_height)

#cleaning variable hand
table(height$hand)

#cleaning variable dad_hand
table(height$dad_hand)
height <- height %>% mutate(dad_hand=ifelse(dad_hand=="r", "R", dad_hand))
height <- height %>% mutate(dad_hand=ifelse(dad_hand=="l", "L", dad_hand))

#cleaning variable mom_hand
table(height$mom_hand)
height <- height %>% mutate(mom_hand=ifelse(mom_hand=="r", "R", mom_hand))

#missing values in weight variable
data_frame$weight <- as.numeric(data_frame$weight)
which(is.na(height$weight))
na.omit(height$weight)
height[is.na(height)] <- 0
view(height)

#cleaning dad_height variable
view(height$dad_height)
height <- height %>% mutate(dad_height=ifelse(dad_height<=69, 70, dad_height))

#cleaning mom_height variable
view(height$mom_height)
height <- height %>% mutate(mom_height=ifelse(mom_height<=60, 70, mom_height))

#creating new variable know as BMI
height <- height %>% mutate(height, weight1=weight*0.4536) 
height <- height %>% mutate(height, height1=height*0.0254)

height <- height %>% mutate(height, BMI=weight1/(height1^2))

#average of BMI and median value
mean(height$BMI)
median(height$BMI)

#creating new variable called BMIcat
height <- height %>% mutate(
  BMIcat=ifelse(BMI<=18.4, "Under", ifelse(BMI>=18.5 & BMI<=24.9, "Normal", "Overweight"))
)

#cleaning sex variable
table(height$sex)
height <- height %>% mutate(sex=ifelse(sex=="Mm", "M", sex))
height <- height %>% mutate(sex=ifelse(sex=="f", "F", sex))

#deleting variable gender
height$gender <- NULL

#generating variable gender
height <- height %>% mutate(gender=ifelse(sex=="M", "Male", "Female"))

#No, of people under, normal and overweight.
table(height$BMIcat)

#question 11
height %>% filter(sex=="F" & BMIcat=="Normal" & age>=30 & hand=="R") %>% count() #to count                   
height %>% filter(sex=="F" & BMIcat=="Normal" & age>=30 & hand=="R") #to view

height %>% percent_rank(12)
                    
L <- 13
R <- 57
height <-height %>% 
  pie(L=left, R=right, main = "hand")
