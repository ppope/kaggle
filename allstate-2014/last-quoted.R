

testing.data <- read.csv("test_v2.csv")

testing.data.ids <- unique(testing.data[[1]])

ind <- rep(0, length(testing.data.ids))

for (i in 1:length(testing.data.ids)) ind[i] <- max(which(testing.data$customer_ID == testing.data.ids[i]))
  

last.quotes <- testing.data[ind, 18:24]


concat.results <- apply(last.quotes, 1, function(x) paste0(x, collapse=""))
#names(results) <- c("A","B","C","D","E","F","G")

final1 <- data.frame(testing.data.ids, concat.results, stringsAsFactors=FALSE)
names(final1) <- c("customer_ID", "plan")

write.csv(final1, file="/home/delores/Desktop/kaggle/submissions/pep-sub3.csv", row.names=FALSE)


