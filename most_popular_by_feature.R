#predicting most popular by some feature:
library(data.table)
library(dplyr)
library(Metrics)

train <- fread("/Users/alexpapiu/Downloads/train.csv", nrow = 1e4)
val <- train[1e5:2e5]


load("/Users/alexpapiu/Documents/Data/expedia.RData")

train %>%
    count(srch_destination_id, hotel_cluster) %>%
    arrange(desc(n)) %>% 
    top_n(5, n) -> top_5

dat <- as.data.table(top_5)
dat[, obs := paste0('y_', 1:.N), by=srch_destination_id]
dcast(dat, srch_destination_id ~ obs, value.var="hotel_cluster") -> top_5


right_join(top_5, train, by = "srch_destination_id") -> temp
temp <- temp[,2:6]
as.list(as.data.frame(t(temp))) -> preds #converts to a list
truth = as.list(train$hotel_cluster)
mapk(5, truth, preds) 
#lots of NA's however.

mapk(5, list(1), list(c(2,4,3,4,1)))



#trying to produce stings for submission:
do.call("paste", temp) -> str_preds
gsub("NA|", "", str_preds) -> str_preds


#for test set:
right_join(top_5, val, by = "srch_destination_id") -> temp
temp <- temp[,2:6]
as.list(as.data.frame(t(temp))) -> preds #converts to a list
truth = as.list(val$hotel_cluster)
mapk(5, truth, preds) #the metric for the competition
#0.2380215 on the 100k test set.


sample <- read.csv("/Users/alexpapiu/Downloads/sample_submission.csv",
                   stringsAsFactors = FALSE)

