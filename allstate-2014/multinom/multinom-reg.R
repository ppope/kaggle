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

#   2/26 
#   There are two interpretations I've thought of for the data analysis task. 
#   The first is predict coverage purchase based on customer data. This is regression.
#   The second is, for a given customer, predict their coverage purchase based on their transaction history.



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
#data[1:9,]

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



####  TEST MODELS ######

#Read in testing data

all.testing.data <- read.csv("test_v2.csv")
all.testing.data.ids <- as.data.frame(unique(all.testing.data[[1]]))
names(all.testing.data.ids) <- "customer_ID"

#For now remove the NA values.
testing.data <- na.omit(testing.data)
testing.data.ids <- unique(testing.data[[1]])

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
final2 <- merge(all.testing.data.ids, final1, all.x=TRUE)


for (i in 1:nrow(final2)){
  
  if (is.na(final2[i,2])){
    final2[i,2] <- "1111111"
  }
  
}

write.csv(final2, file="/home/delores/Desktop/kaggle/submissions/pep-sub1.csv", row.names=FALSE)







