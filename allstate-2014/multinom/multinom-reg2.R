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


#   This script builds a model on only the purchase data (record_type=1) and then predicts for the training set. 
#   This script utilizes an imputed data set for the training set.

library(nnet)

directory <- "/home/delores/Desktop/kaggle/data"
#set working directory
setwd(directory)
#read in the data
data <- read.csv("train.csv")
ind <- which(data$record_type == 1)
purchase.data <- data[ind,]

#####   MULTINOMIAL REGRESSION  ######

#Note that multinomial regression doesn't work with the whole data set.
#start <- proc.time()
#model.mu <- multinom( (A + B + C + D + E + F + G)  ~ group_size + homeowner + car_age + car_value  + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=purchase.data, model=TRUE)
#runtime <- proc.time() - start

#   user  system elapsed 
#   71.941   0.052  72.109 
#coef(model.mu)
#Why am I getting a table of coefficients?

#####   MODELS  ######
#Train models on purchase data only.

model.mu.A <- multinom( A ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=purchase.data, model=TRUE)
model.mu.B <- multinom( B ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=purchase.data, model=TRUE)
model.mu.C <- multinom( C ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=purchase.data, model=TRUE)
model.mu.D <- multinom( D ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=purchase.data, model=TRUE)
model.mu.E <- multinom( E ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=purchase.data, model=TRUE)
model.mu.F <- multinom( F ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=purchase.data, model=TRUE)
model.mu.G <- multinom( G ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=purchase.data, model=TRUE)







#####  TEST MODELS ######

#Read in testing data

testing.data <- read.csv("test_v2-imp1.csv")
testing.data.ids <- unique(testing.data[[1]])
names(testing.data.ids) <- "customer_ID"

#Arbitrarily remove multiple entries per customer ID and remove "useless" data. 
relevant.testing.data <- testing.data[match(testing.data.ids, testing.data$customer_ID), c("group_size", "homeowner", "car_age", "risk_factor", "age_oldest", "age_youngest", "married_couple", "C_previous", "duration_previous", "cost")]


#This function takes as input a multinomial model and data for the model to be tested and outputs predictions.
predictMNL <- function(model, newdata) {
  
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
    
    # Draw random values
    vals <- runif(nrow(newdata))
    
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
    
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
    
    # Return the values
    return(ids)
  }
}



results.A <- predictMNL(model.mu.A, relevant.testing.data)
results.B <- as.numeric(predict(model.mu.B, relevant.testing.data)) - 1
results.C <- predictMNL(model.mu.C, relevant.testing.data)
results.D <- predictMNL(model.mu.D, relevant.testing.data)
results.E <- as.numeric(predict(model.mu.E, relevant.testing.data)) - 1
results.F <- predictMNL(model.mu.F, relevant.testing.data)
results.G <- predictMNL(model.mu.G, relevant.testing.data)



results <- cbind(results.A, results.B, results.C, results.D, results.E, results.F, results.G)

concat.results <- apply(results, 1, function(x) paste0(x, collapse=""))
#names(results) <- c("A","B","C","D","E","F","G")

final1 <- data.frame(testing.data.ids, concat.results, stringsAsFactors=FALSE)
names(final1) <- c("customer_ID", "plan")

write.csv(final1, file="/home/delores/Desktop/kaggle/submissions/pep-sub2.csv", row.names=FALSE)







