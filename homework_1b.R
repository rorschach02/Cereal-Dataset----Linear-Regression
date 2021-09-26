#########################################################
## Created By : Aniket Maheshwari
## Date: 09/07/2021
## Aim is to do some Exploratory Data Analysis on Cereal data set and prepare a clean data for further regression. 
#########################################################


##Clear the environment 
rm(list = ls())


## First we will set the directory of the R script 
setwd("C:/Users/anike/Desktop/Sem 1/EAS 506 Statistical Data Mining/Homework/Homework 1")



#######################################
## Loading our saved data 
#######################################

load("C:/Users/anike/Desktop/Sem 1/EAS 506 Statistical Data Mining/Homework/Homework 1/cleaned_cereal_data.RData")


dim(Data4)

library(MASS)
## install.packages("ISLR")
library(ISLR)
############################################
### Co-relation 
num_columns <- unlist(lapply(Data4, is.numeric)) ## returns a Boolean Vector telling which of our 
data_numeric <- subset(Data4[,num_columns])
corrcereal <- cor(data_numeric)
corrcereal


## install.packages("corrplot")
library(corrplot)
corrplot(corrcereal , method = 'number' , main = "Correlation Plot" )


###########################################
## Q.1) Which predictors appear to have a significant relationship to the response 'rating'? 

### By co-relation plot we can say that : 
## 'calories' and 'sugars' are highly negatively co-related to our response 'rating' feature. 
##  'protein' , 'fiber' and 'potass' are positively co-related to our response 'rating' feature.



##########################################
# install.packages("ElemStatLearn")
#library(ElemStatLearn)

colnames(Data4)

### Fitting a linear regression model with all the features. 

linear_model_third <- lm(rating ~  calories + protein + fat + sodium + fiber + carbo + sugars + potass + vitamins + weight , data = Data4)  
linear_model_third_summary <- summary(linear_model_third)
linear_model_third_summary
### Residual standard error: 3.058e-07 on 62 degrees of freedom
## Here , it indicates that 'weight' feature is not important so we'll fit next model without that feature.

##Fitting linear regression model with updated features.
linear_model_fifth <- lm(rating ~  calories + protein + fat + sodium + fiber + carbo + sugars + potass + vitamins , data = Data4)
linear_model_fifth_summary <- summary(linear_model_fifth)
linear_model_fifth_summary
### Residual standard error: 3.064e-07 on 63 degrees of freedom

## Fitting linear regression model with features that are negatively correlated to 'rating' (our response)
linear_model_first <- lm(rating ~ calories + sugars + fat +sodium , data = Data4)
linear_model_first_summary <- summary(linear_model_first)
linear_model_first_summary
## Residual standard error: 6.25 on 68 degrees of freedom
## This suggests that calories and fat is not important feature which is surprising because in corelation plot 'calories' had -0.64 corelation with 'rating' feature. 


## Fitting linear regression model with features that are positively correlated to 'rating' (our response)
linear_model_second <- lm(rating ~ protein + fiber + potass  , data = Data4)
linear_model_second_summary <- summary(linear_model_second)
linear_model_second_summary 
## Residual standard error: 9.358 on 69 degrees of freedom


############################################
###Multi-linear transformation / Interactions

############################################

## Interactions with all features
## using "*" 
linear_model_fourth <- lm(rating ~  calories * protein * fat * sodium  *carbo  * sugars    , data = Data4)  
linear_model_fourth_summary <- summary(linear_model_fourth)
linear_model_fourth_summary
#### Residual standard error: 1.347 on 9 degrees of freedom
### significant relations: 
# fat:sodium:carbo:sugars :  0.000666 ***
# calories:protein:fat:sodium:sugars : 0.000997 ***
# calories:fat:sodium:carbo:sugars  : 0.000665 ***




## using ":"  with most significantly impacting features to our response
linear_model_sixth <- lm(rating ~ calories:sugars +  protein:carbo:potass , data = Data4)
linear_model_sixth_summary <- summary(linear_model_sixth)
linear_model_sixth_summary
## Residual standard error: 6.671 on 70 degrees of freedom
## Significant relations : 
# calories:sugars      : < 2e-16
# protein:carbo:potass : < 2e-16

