library(data.table)
library(dplyr)

n = 5e6
train <- fread("/Users/alexpapiu/Documents/Data/Expedia/train.csv", nrow = n)

set.seed(1243)
sam = sample(1:n, n/8)
val = train[sam,]
train = train[-sam,]

train <- fread("/Users/alexpapiu/Documents/Data/Expedia/train.csv", nrow = n)
test <- fread("/Users/alexpapiu/Documents/Data/Expedia/test.csv")
sample <- fread("/Users/alexpapiu/Downloads/sample_submission.csv") 

train %>%
    count(srch_destination_id, hotel_cluster) %>%
    arrange(desc(n)) %>% 
    top_n(5, n) %>% 
    mutate(obs = paste0("y_", 1:n())) %>% 
    select(-n) %>% 
    spread(obs, hotel_cluster) %>% 
    select(srch_destination_id, y_1, y_2, y_3, y_4, y_5) -> top_5

#for test set:
right_join(top_5, test, by = "srch_destination_id") -> temp
preds <-  temp[,2:6]

#transforms the top 5 in the correct format for prediction:
apply(preds, 1, function(x) {
    paste(x, collapse = " ")
}) -> new_preds

gsub(pattern = "NA", replacement = "91", x = new_preds) -> solution 
# just put 91 instead of NA's as the most popular

solution = data.frame(id = sample$id, hotel_cluster = solution)

data.table::fwrite(solution, "first_try.csv")
