# This script is the same as multinom-reg2.R but instead usings 5 imputed data sets for both training and testing.

# Five models are trained, which are then evenly combined into one.

library(nnet)

directory <- "/home/delores/Desktop/kaggle/data"
#set working directory
setwd(directory)
#read in the data
training.data1 <- read.csv("imputed-purchase-training-data1.csv")
training.data2 <- read.csv("imputed-purchase-training-data2.csv")
training.data3 <- read.csv("imputed-purchase-training-data3.csv")
training.data4 <- read.csv("imputed-purchase-training-data4.csv")
training.data5 <- read.csv("imputed-purchase-training-data5.csv")

training.data.list <- list(training.data1, training.data2, training.data3, training.data4, training.data5)

#####   MULTINOMIAL REGRESSION  ######

#####   MODELS  ######
#Train models on purchase data only.

# Models list is a nested list, a list of length 5 wherein each element is a list of length 7.
# The first level corresponds to one of the five imputed training sets.
# The second level corresponds to one of the seven insurance options A,B,C,D,E,F,G.
models.list <- vector("list", length=5)

start <- proc.time()

for (i in 1:length(models.list)){
  
  model.mu.A <- multinom( A ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=training.data.list[[i]], model=TRUE)
  model.mu.B <- multinom( B ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=training.data.list[[i]], model=TRUE)
  model.mu.C <- multinom( C ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=training.data.list[[i]], model=TRUE)
  model.mu.D <- multinom( D ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=training.data.list[[i]], model=TRUE)
  model.mu.E <- multinom( E ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=training.data.list[[i]], model=TRUE)
  model.mu.F <- multinom( F ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=training.data.list[[i]], model=TRUE)
  model.mu.G <- multinom( G ~ group_size + homeowner + car_age + risk_factor + age_oldest + age_youngest + married_couple + C_previous + duration_previous + cost, data=training.data.list[[i]], model=TRUE)
  
  temp.model.list <- list(model.mu.A, model.mu.B, model.mu.C, model.mu.D, model.mu.E, model.mu.F, model.mu.G)
  
  models.list[[i]] <- temp.model.list
  
}

stop <- proc.time() - start 

# Runtime varies but a sample is:
# user  system elapsed 
# 220.857   0.548 221.83

#####  COMBINE MODELS ######

# Syntax for predict() is: predict(model, data, "probs")
# model.probs.list <- vector("list", length=5)
# for (i in 1:length(training.data.list)) model.probs.list[[i]] <- lapply(models.list[[i]], function(x) predict(x, training.data.list[[i]], "probs")) 
# ensemble.model.probs.list <- vector("list", length=7)
# for (i in 1:7) ensemble.model.probs.list[[i]] <- (model.probs.list[[1]][[i]] + model.probs.list[[2]][[i]] + model.probs.list[[3]][[i]] + model.probs.list[[4]][[i]] + model.probs.list[[5]][[i]])/5 

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







