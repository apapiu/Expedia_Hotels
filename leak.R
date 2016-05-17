#leak:

train$leak = paste(train$orig_destination_distance,train$user_location_city)
val$leak = paste(val$orig_destination_distance,val$user_location_city)


#the hotel cluster is indeed always the same:
train %>% 
    filter(orig_destination_distance != "NA") %>% 
    group_by(leak) %>% 
    summarize(clusterz = unique(hotel_cluster)) -> leaks

train %>% 
    filter(orig_destination_distance != "NA") %>% 
    count(leak, hotel_cluster) %>%
    arrange(desc(n)) %>% 
    mutate(obs = paste0("y_", 1:n())) %>% 
    select(-n) %>% 
    spread(obs, hotel_cluster) %>% 
    select(leak, y_1) -> leaks

leaks$leak %>% unique() %>% length()

#this gives you the predictions for the test set that are almost 100% correct:
#right_join(leaks, val[is.na(orig_destination_distance) == FALSE,], by = "leak")-> new_val 


right_join(leaks, val, by = "leak")-> new_val 
