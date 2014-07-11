# Data Analysis Objective: Predict a purchased policy based on transaction history
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
#   For the classifier, are we allowed to select a subset of the data? Does subsetting the data conflict with how the classifier is judged?


library(nnet)

#Set your directory here
directory <- "/home/delores/Desktop/kaggle/data"

#set working directory
setwd(directory)
#read in the data
data <- read.csv("train.csv")


#view the first nine rows
data[1:9,]

ind <- which(data$record_type == 1)
purchase.data <- data[ind,]

#####   MULTINOMIAL REGRESSION  ######

#Note that multinomial regression doesn't work with the whole data set.
#model.mu.A <- multinom(A ~ group_size + record_type + homeowner + car_age + car_value  + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=data)

start <- proc.time()
model.mu <- multinom( (A + B + C + D + E + F + G)  ~ group_size + homeowner + car_age + car_value  + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=purchase.data, model=TRUE)
runtime <- proc.time() - start

#   user  system elapsed 
#   71.941   0.052  72.109 

coef(model.mu)
#Why am I getting a table of coefficients?


model.mu.A <- multinom( A ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=purchase.data, model=TRUE)

coef(model.mu.A)
test <- purchase.data[1, c("group_size", "homeowner", "car_age", "risk_factor", "age_oldest",  "age_youngest", "married_couple", "C_previous", "duration_previous", "cost")]

coef(model.mu.A)*c(1,test)







