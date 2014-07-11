
require(Amelia)

directory <- "/home/delores/Desktop/kaggle/data"
#set working directory
setwd(directory)
#read in the data
training.data <- read.csv("train.csv")
testing.data <- read.csv("test_v2.csv")


###### IMPUTE TESTING SET ######

# As a first pass we first impute the data in the training set corresponding to the purchase records.
ind <- which(training.data$record_type == 1)
purchase.training.data <- training.data[ind, ]

# Remove noise variables
purchase.training.data <- purchase.training.data[-c(2,3,4,5,6,7)]
imputed.purchase.training.data <- amelia(purchase.training.data, m=5, noms=c( "car_value"))
write.amelia(obj=imputed.purchase.training.data, file.stem = "imputed-purchase-training-data", quote=FALSE, row.names=FALSE)



###### IMPUTE TRAINING SET ######

#Again remove noise variables
testing.data2 <- testing.data[-c(2,3,4,5,6,7)]
testing.data.imputed <- amelia(testing.data2, m=5, noms=c( "car_value"))
write.amelia(obj=testing.data.imputed, file.stem = "imputed-purchase-testing-data", quote=FALSE, row.names=FALSE)