#predicting by binned distance.
train %>% 
    filter(!is.na(orig_destination_distance)) -> train_small

train_small %>%
    count(dist_bin, hotel_cluster) %>%
    arrange(desc(n)) %>% 
    top_n(5, n) -> top_5

dat <- as.data.table(top_5)
dat[, obs := paste0('y_', 1:.N), by=dist_bin]
dcast(dat, dist_bin ~ obs, value.var="hotel_cluster") -> top_5

right_join(top_5, train_small, by = "dist_bin") -> temp
temp <- temp[,2:6]
as.list(as.data.frame(t(temp))) -> preds #converts to a list
truth = as.list(train_small$hotel_cluster)
mapk(5, truth, preds) 