#this script combines different features by counts and then takes a weighted average to 
#predict most popular 5 clusters

load("/Users/alexpapiu/Documents/Data/expedia.RData")
truth = as.list(val$hotel_cluster) #we need this for the validation

#only select stuff we're interested in:
small_val <- select(val, srch_destination_id, user_location_city)


#top counts for different variables:
train %>%
    count(user_location_city, hotel_cluster) %>%
    arrange(desc(n)) %>%
    spread(hotel_cluster, n, fill = 0) -> hotel_continent

small_val_cont <- right_join(hotel_continent, small_val,
                             by = "user_location_city")


train %>%
    count(srch_destination_id, hotel_cluster) %>%
    arrange(desc(n)) %>%
    spread(hotel_cluster, n, fill = 0) -> srch_destination_id

small_val_srch_id <- right_join(srch_destination_id, small_val, 
                        by = "srch_destination_id")

#makes the counts numeric and then fractions:
X_1 = as.matrix(select(small_val_cont, - user_location_city, srch_destination_id))[,1:100]
X_2 = as.matrix(select(small_val_srch_id, - srch_destination_id, user_location_city))[,1:100]

X_1 = X_1/rowSums(X_1)
X_2 = X_2/rowSums(X_2)

#top 5 clusters - this is pretty slow:
lapply(1:dim(X_1)[1], function(i) {
    count = 0.1815508*X_1[i,] + 0.3121058*X_2[i,] #the weights based on mapk
    sort(count, decreasing = TRUE)[1:5] %>% names() %>% as.numeric()
}) -> preds

mapk(5, truth, preds) #the metric for the competition


