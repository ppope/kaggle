
directory <- "/home/delores/Desktop/kaggle/data"

setwd(directory)


######    LOGISTIC REGRESSION    ######


######        TRAIN MODEL        ######

train <- read.csv("imputed-train.csv")
#train <- read.csv("/home/delores/Desktop/kaggle/data/imputed-train.csv")

#Remove customer_ID, shopping_pt, and imp variables.
train <- train[-c(which(names(train) == "customer_ID"), which(names(train) == "shopping_pt"), which(names(train) == "imp"))]
model.log <- glm(record_type ~ ., family = "binomial", data = train)

# Predict the probabilities
# predict(model, new.df) will just give you the logit (z) values
# We want the probabilities so use the "response" type
train.probs <- predict(model.log, train, "response")
# Draw some random values between 0 and 1
train.draws <- runif(nrow(train))
# Check if draw value is less than threshold probability
train.results <- (draws < probs)
error <- (sum(as.numeric(train.results) == train$record_type) / nrow(train))

# For this draw:
# > error
# [1] 0.7517882

######     CLEAR WORKSPACE       ######

rm(list=c("train", "train.probs", "train.results", "train.draws", "train.error", "directory"))


######        TEST MODEL         ######

test <- read.csv("imputed-test.csv")

# Remove customer_ID, shopping_pt, and imp variables.
test <- test[-c(which(names(test) == "customer_ID"), which(names(test) == "shopping_pt"), which(names(test) == "imp"))]

# Calculate probabilites for each 
test.probs <- predict(model.log, test, "response")


######     CLEAR WORKSPACE       ######

rm("model.log")
gc()

######    COMBINE RESULTS        ######

test <- read.csv("test_v2.csv")

customer.IDs <- test$customer_ID

N <- length(customer.IDs)

# Dataframe containing the probabilities of each coverage predicted from each of the imputed datasets.
x <- data.frame(test.probs[1:N], test.probs[(N+1):(2*N)], test.probs[(2*N+1):(3*N)])
mean.probs <- rowMeans(x)
concat.quotes <- apply(test[, 18:24], 1, function(x) paste0(x, collapse=""))
results.df <- data.frame(customer.IDs, mean.probs, as.character(concat.quotes))
M <- length(unique(customer.IDs))
preds <- data.frame("customer_ID" = unique(customer.IDs), "plan" = rep(0,M))

for (i in 1:M){
  
  current.ID.ind <- which(results.df$customer.IDs == unique(customer.IDs)[i])
  preds[i,2] <-  as.character(results.df[current.ID.ind[which.max(results.df[current.ID.ind, "mean.probs" ])], 3])
  
}


setwd("/home/delores/Desktop/kaggle/submissions/")
write.csv(preds, "pep-sub4.csv", row.names=FALSE)
