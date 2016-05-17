#predicting most popular by some feature:
library(data.table)
library(dplyr)
library(tidyr)
library(Metrics)
library(lubridate)

#load("/Users/alexpapiu/Documents/Data/expedia.RData")
load("validation.RData")

#n = 5e6
#train <- fread("/Users/alexpapiu/Documents/Data/Expedia/train.csv", nrow = n)
#train$time <- as.numeric(as.POSIXct(train$date_time))
#train$time <- train$time/10e6
#train$year <- year(train$date_time)

#split based on time, and only do validation for booking == 1,
#val <- train[train$time > 140 & train$is_booking == 1,]
#train <- train[train$time < 140,]


#~~~~~~~~~~~~~~
#top clusters for given feature:
train %>%
    count(orig_destination_distance, hotel_cluster) %>%
    arrange(desc(n)) %>% 
    mutate(obs = paste0("y_", 1:n())) %>% 
    select(-n) %>% 
    spread(obs, hotel_cluster) %>% 
    select(orig_destination_distance, y_1, y_2, y_3, y_4, y_5) -> top_5

truth = as.list(val$hotel_cluster) #we need this for the validation

#for validation set:
right_join(top_5, val, by = "orig_destination_distance") -> temp


preds <-  temp[,2:6]

#exploit leak:
#answers = new_val$y_1
#indices = which(!is.na(answers))
#preds$y_1[indices] = answers[indices]

#predict:
as.list(as.data.frame(t(preds))) -> preds #converts to a list
mapk(5, truth, preds) #the metric for the competition

#0.3677931 for the leak with 5e6 data
#(0.2897957 w/o leak)


#0.2995713 for the leak with 1e6 data.


#0.2601897 on srch_destination_id
#0.06042841 on user_location_city
#0.1081442 hotel_continent
#0.1425041 for hotel_country

#how to combine these predictions?
#maybe feed them into xgboost once we're done.

train %>%
    count(user_location_city, hotel_cluster) %>%
    arrange(desc(n)) %>% 
    top_n(5, n) %>% 
    mutate(obs = paste0("y_", 1:n())) %>% 
    select(-n) %>% 
    spread(obs, hotel_cluster) %>% 
    select(user_location_city, y_1, y_2, y_3, y_4, y_5) -> top_5


#for validation set:
right_join(top_5, val, by = "user_location_city") -> temp
preds <- temp[,2:6]

#replace NA's with the continent vote:
preds_vect <- as.numeric(as.matrix(preds))
preds_cont_vect <- as.numeric(as.matrix(preds_city))
preds_vect[is.na(preds_vect)] <- preds_cont_vect[is.na(preds_vect)]

#preds_vect[is.na(preds_vect)] <- sample(100)

preds <- matrix(preds_vect, nrow = nrow(temp))

as.list(as.data.frame(t(preds))) -> preds #converts to a list
mapk(5, truth, preds) #the metric for the competition

#0.3152217 slight improvmnt.



