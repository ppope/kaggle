
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

# Draw some random values between 0 and 1
test.draws <- runif(nrow(test))
# Check if draw value is less than threshold probability
test.results <- as.numeric(test.draws < test.probs)


######     CLEAR WORKSPACE       ######

rm(list=c("test", "model.log", "test.probs", "test.draws"))

######    COMBINE RESULTS        ######

test <- read.csv("test_v2.csv")

customer.IDs <- test$customer_ID

N <- length(customer.IDs)

# Dataframe containing the predicitions from each of the imputed datasets.
x <- data.frame(test.results[1:N], test.results[(N+1):(2*N)], test.results[(2*N+1):(3*N)])



combined.pred <- round(rowMeans(x))
concat.quotes <- apply(test[, 18:24], 1, function(x) paste0(x, collapse=""))
combined.results <- data.frame(customer.IDs, combined.pred, concat.quotes)



