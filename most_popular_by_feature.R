#predicting most popular by some feature:
library(data.table)
library(dplyr)
library(tidyr)
library(Metrics)

load("/Users/alexpapiu/Documents/Data/expedia.RData")

truth = as.list(val$hotel_cluster) #we need this for the validation

#top clusters for given feature:
train %>%
    count(hotel_continent, hotel_cluster) %>%
    arrange(desc(n)) %>% 
    top_n(5, n) %>% 
    mutate(obs = paste0("y_", 1:n())) %>% 
    select(-n) %>% 
    spread(obs, hotel_cluster) %>% 
    select(hotel_continent, y_1, y_2, y_3, y_4, y_5) -> top_5


#for validation set:
right_join(top_5, val, by = "hotel_continent") -> temp
preds_city <- temp[,2:6]
preds <-  temp[,2:6]
as.list(as.data.frame(t(preds))) -> preds #converts to a list
mapk(5, truth, preds) #the metric for the competition

# 0.3121058 for srch_destination_type_id - maybe overfitting?
# 0.1145858 for hotel_continent
# 0.1815508 for user_location_city
#0.07081833 for is_package
#0.05839083 for is_booking

#how to combine these predictions?
#maybe feed them into xgboost once we're done.

train %>%
    count(srch_destination_id, hotel_cluster) %>%
    arrange(desc(n)) %>% 
    top_n(5, n) %>% 
    mutate(obs = paste0("y_", 1:n())) %>% 
    select(-n) %>% 
    spread(obs, hotel_cluster) %>% 
    select(srch_destination_id, y_1, y_2, y_3, y_4, y_5) -> top_5


#for validation set:
right_join(top_5, val, by = "srch_destination_id") -> temp
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




