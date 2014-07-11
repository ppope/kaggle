

require(randomForest)
directory <- "/home/delores/Desktop/kaggle/data"
setwd(directory)

######        TRAIN MODEL        ######

train <- read.csv("imputed-train.csv")
#train <- read.csv("/home/delores/Desktop/kaggle/data/imputed-train.csv")

#Remove customer_ID, shopping_pt, and imp variables.
train <- train[-c(which(names(train) == "customer_ID"), which(names(train) == "shopping_pt"), which(names(train) == "imp"))]


fit.rf = randomForest(record_type ~ ., family = "binomial", data = train, ntree = 1, proximity = FALSE)
