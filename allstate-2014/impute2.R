
directory <- "/home/delores/Desktop/kaggle/data"

setwd(directory)


######    IMPUTE TRAINING DATA (AGAIN) M=3  ######

# This time on the entire dataset, leaving out variables time and state, and stacking them into one data frame.

require(Amelia)

training.data <- read.csv("train.csv")
training.data <- training.data[-c(which(names(training.data) == "time"), which(names(training.data) == "state"))]
imputed.training.data <- amelia(training.data, m=3, noms=c( "car_value"))
write.amelia(obj=imputed.training.data, file.stem = "imputed-train", separate=FALSE, orig.data=FALSE, row.names=FALSE)


rm(list=ls())
gc()

testing.data <- read.csv("test_v2.csv")
testing.data <- testing.data[-c(which(names(testing.data) == "time"), which(names(testing.data) == "state"), which(names(testing.data) == "record_type"))]
imputed.testing.data <- amelia(testing.data, m=3, noms=c( "car_value"))
write.amelia(obj=imputed.testing.data, file.stem = "imputed-test", separate=FALSE, orig.data=FALSE, row.names=FALSE)


