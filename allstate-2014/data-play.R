#   Data Analysis Objective: Predict a purchased policy based on transaction history
#   The goal in this project is to predict the pattern of coverage bought, 
#     e.g. (A,B,C,D,E,F,G)=(1,0,2,2,1,2,2) 
#   based on transaction history, 
#     e.g. time, state, location, group_size, homeowner, car_age, car_value, risk_factor, age_oldest, age_youngest, married_couple, C_previous, duration_previous 


#   Download the data from 
#   https://www.kaggle.com/c/allstate-purchase-prediction-challenge/data

#Resources
#   http://www.r-bloggers.com/how-to-multinomial-regression-models-in-r/

#Notes
#   For each customer_ID, you are given their quote history. 
#   In the training set you have the entire quote history, the last row of which contains the coverage options they purchased. 
#   What coverage they purchased is indicated by record_type=1
#   Some customer characteristics may change over time (e.g. as the customer changes or provides new information), 
#   The cost depends on both the product and the customer characteristics. Thus cost is correlated with other variables.
#   A customer may represent a collection of people, as policies can cover more than one person.
#   A customer may purchase a product that was not viewed!

#Questions
#   For my classifier, am I allowed to select a subset of the data? Does subsetting the data conflict with how the classifier is judged?
#   I should be able to choose what data I want to run. E.g. choose only the highest 
#   Should I train my classifer on all the data or

#   2/26 
#   There are two interpretations I've thought of for the data analysis task. 
#   The first is predict coverage purchase based on customer data. This is regression.
#   The second is, for a given customer, predict their coverage purchase based on their transaction history.
#   The second seems more along the lines of the competition, although I will entertain the first to see what results I can get.


#Idea: 
#   1. Build a model with the purchase data (e.g. record_type=1) using multinomial regression to predict coverge.
#   2. Process the training data by filling in NA values.

library(nnet)

directory <- "/home/delores/Desktop/kaggle/data"
#set working directory
setwd(directory)
#read in the data
data <- read.csv("train.csv")


#view the first nine rows
data[1:9,]

ind <- which(data$record_type == 1)
purchase.data <- data[ind,]

purchase.data2 <- na.omit(purchase.data[,c(6,18:24)])
test <- split(purchase.data2, f = purchase.data$state)
test2 <- lapply(test, function(x) x[-1])
test3 <- lapply(test2, colMeans)


#####   GLMS    ######

#Here I selected variables that seemed most relevant to the task. That is, I selected them in a totally subjective way.
model1 <- glm( (A + B + C + D + E + F + G) ~ (group_size + record_type + homeowner + car_age + car_value 
                                              + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost), family = gaussian, data=data)
summary(model1)

model1.binomial <- glm( (A + B + C + D + E + F + G) ~ (group_size + record_type + homeowner + car_age + car_value 
                                              + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost), family = binomial, data=data)
summary(model1)

#In this model the previous variables and well as "state" and "location" were run. I tested adding further variables, i.e.
model2.binomial <- glm( (A + B + C + D + E + F + G) ~ (state + location + group_size + record_type + homeowner + car_age + car_value 
                                              + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost), family = binomial, data=data)
summary(model2.binomial)

#In this model the previous variables and well as "state" and "location" were run. I tested adding further variables, i.e.
model2 <- glm( (A + B + C + D + E + F + G) ~ (state + location + group_size + record_type + homeowner + car_age + car_value 
                                              + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost), family = gaussian, data=data)
summary(model2)




