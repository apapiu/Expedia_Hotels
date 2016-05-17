#this script combines different features by counts and then takes a weighted average to 
#predict most popular 5 clusters

load("/Users/alexpapiu/Documents/Data/expedia.RData")
truth = as.list(val$hotel_cluster) #we need this for the validation

small_val <- select(val, hotel_continent, srch_destination_id)


#top counts for different variables:
train %>%
    count(hotel_continent, hotel_cluster) %>%
    arrange(desc(n)) %>%
    spread(hotel_cluster, n, fill = 0) -> hotel_continent

#a more efficient way to keep track:
X = hotel_continent[,2:100, with = FALSE]
X = X/rowSums(X)
rownames(X) <- hotel_continent$hotel_continent



small_val_cont <- right_join(hotel_continent, small_val,
                             by = "hotel_continent")


train %>%
    count(srch_destination_id, hotel_cluster) %>%
    arrange(desc(n)) %>%
    spread(hotel_cluster, n, fill = 0) -> srch_destination_id

X1 = srch_destination_id[,2:100, with = FALSE]
X1 = X1/rowSums(X1)
rownames(X1) <- srch_destination_id$srch_destination_id


small_val_srch_id <- right_join(srch_destination_id, small_val, 
                                by = "srch_destination_id")

#makes the counts numeric and then fractions:
X_1 = as.matrix(select(small_val_cont, - hotel_continent, srch_destination_id))[,1:100]
X_2 = as.matrix(select(small_val_srch_id, - srch_destination_id, hotel_continent))[,1:100]

X_1 = X_1/rowSums(X_1)
X_2 = X_2/rowSums(X_2)

#top 5 clusters - this is pretty slow:
lapply(1:dim(X_1)[1], function(i) {
    count = 0.1815508*X_1[i,] + 0.3121058*X_2[i,] #the weights based on mapk
    sort(count, decreasing = TRUE)[1:5] %>% names() %>% as.numeric()
}) -> preds

mapk(5, truth, preds) #the metric for the competition

