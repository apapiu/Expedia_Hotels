#xgboost:
#the issue here is that there are too many factor levels:

library(xgboost)

train %>% 
    transmute(#is_mobile,
              #is_package,
              #is_booking,
              #posa_continent = as.factor(posa_continent),
              hotel_continent = as.factor(hotel_continent)) -> new_train
              #srch_destination_type_id =
              #    as.factor(srch_destination_type_id),
              #srch_rm_cnt,
              #srch_adults_cnt) -> new_train

X <- model.matrix(~., data = new_train)

y <- as.numeric(train$hotel_cluster)

model_xgb <- xgboost(X, y, nrounds = 30,
                     objective = "multi:softprob",
                     num_class = 100)

xgb.importance(model = model_xgb, feature_names = colnames(X)) -> xgb.importance
#hotel_continent, srch_adults_cnt, is_package, srch_rm_cnt - are important.


#validation set:

val %>% transmute(hotel_continent = as.factor(hotel_continent)) -> new_val
X_val = model.matrix(~., data = new_val)

predict(model_xgb, X_val) -> predictions
solution <- t(matrix(predictions, nrow = 100))
colnames(solution)  <- 1:100 #you need a key here 

apply(solution, 1, function(x) {
    sort(x, decreasing = TRUE)[1:5] %>% names() %>% as.numeric()}) -> xgb_preds
#this gets the top 5 probabilities from xgboost.

as.list(as.data.frame(xgb_preds)) -> xgb_preds #converts to a list
mapk(5, truth, xgb_preds) #the metric for the competition

#0.02214 for hotel_continent 
#- terrible, 1/5 than what you get if you predict top5 clusters.



