######    TEST      ######

# Combine results from log-reg2 and last quoted. 

setwd("/home/delores/Desktop/kaggle/submissions")
last.quoted <- read.csv("pep-sub3.csv")
log.reg <- read.csv("pep-sub4.csv")

#Find what entries differ in the two data frames, and randomly select one of them in the new data frame.

x <- which(log.reg$plan != last.quoted$plan)
combined <- last.quoted

#Use the probs parameter to change weighting between last quoted and logistic regression results.
draws <- sample(c(0,1), length(x), replace=TRUE, prob = c(.90,.10))
for (i in 1:length(x)) if (draws[i] == 1) combined[x[i],2] <- log.reg[x[i],2]


# Need to leftpad with zeroes

for (i in 1:nrow(combined)) {
  while(nchar(combined$plan[i]) != 7){
    combined$plan[i] <- paste0("0", combined$plan[i])
  }       
}

write.csv(combined, file="/home/delores/Desktop/kaggle/submissions/pep-sub6.csv", row.names=FALSE)

