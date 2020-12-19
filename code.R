if(require(tidyverse)) library(tidyverse)
if(require(caret)) library(caret)
if(require(broom)) library(broom)
if(require(lubridate)) library(lubridate)
if(require(ggrepel)) library(ggrepel)
if(require(gam)) library(gam)
if(require(splines)) library(splines)
if(require(foreach)) library(foreach)

#loss function
RMSE <- function(true_ratings, predicted_ratings){sqrt(mean((true_ratings - predicted_ratings)^2))}

#loads ibov
td <- tempdir()
ibov <- tempfile(tmpdir = td, fileext = ".rda")
download.file("https://github.com/caerebello/ibovrange/raw/main/rdas/ibov.rda", ibov)
load(ibov)
unlink(td)
rm(td)

#Selects items to change the date, renames high and low and makes an index
for_preds <- ibov %>% select(date, close, high, low, volM) %>% rename(`high_-1` = high, `low_-1` = low) %>% mutate(ind = 1:n())
#divides into two data sets
for_preds_1 <- for_preds %>% select(date, ind)
for_preds_2 <- for_preds %>% select(-date)
#changes the date by index
for_preds_2$ind <- for_preds_2$ind - 1
#consolidates into a dataset with the selected predictors and removes NAs
for_preds <- for_preds_1 %>% left_join(for_preds_2, by = "ind") %>% select(-ind) 
ibov_preds <- ibov %>% select(-close, -volM) %>% left_join(for_preds, by = "date") %>% 
  select(-year, -quarter, -selic, -dax, -nasdaq, -sp500, -nikkei, -sse, -usd, -ipca, -igpm, -ipca_acc, -var, -lse, -ieci)
ibov_preds <- na.omit(ibov_preds)
#divides dataset into train_set and test_set
test_index <- createDataPartition(y = ibov_preds$high, times = 1, p = 0.1, list = FALSE)
train_set <- ibov_preds[-test_index,]
test_set <- ibov_preds[test_index,]
#save(train_set, file = "rdas/train-set.rda")
#save(test_set, file = "rdas/test-set.rda")

#Logistic Regression
fit_glm_high <- glm(high ~ . -low, train_set, family = "gaussian") #fit glm for high
fit_glm_low <- glm(low ~ . -high, train_set, family = "gaussian") #fit glm for low
#save(fit_glm_high, file = "rdas/fit-glm-high.rda")
#save(fit_glm_low, file = "rdas/fit-glm-low.rda")
#makes predictions
glm_pred_high <- predict(fit_glm_high, test_set)
glm_pred_low <- predict(fit_glm_low, test_set)
#with 1 rmse
#calculates margin of error
glm_rmse_low <- RMSE(glm_pred_low, test_set$low) 
glm_rmse_high <- RMSE(glm_pred_high, test_set$high)
# makes intervals
glm_pred_low <- data.frame(pred = glm_pred_low, max = glm_pred_low + glm_rmse_low, min = glm_pred_low - glm_rmse_low)
glm_pred_high <- data.frame(pred = glm_pred_high, max = glm_pred_high + glm_rmse_high, min = glm_pred_high - glm_rmse_high)
#calculates accuracy
glm_accuracy_low1 <- mean(glm_pred_low$min <= test_set$low & glm_pred_low$max >= test_set$low)
glm_accuracy_high1 <- mean(glm_pred_high$min <= test_set$high & glm_pred_high$max >= test_set$high)
glm_total_accuracy1 <- mean(glm_pred_high$min <= test_set$high & glm_pred_high$max >= test_set$high & glm_pred_low$min <= test_set$low & glm_pred_low$max >= test_set$low)
glm_partial_accuracy1 <- mean((glm_pred_high$min <= test_set$high & glm_pred_high$max >= test_set$high) | (glm_pred_low$min <= test_set$low & glm_pred_low$max >= test_set$low))
#calculates trading margin
m_h <- mean(glm_pred_high$pred)
m_l <- mean(glm_pred_low$pred)
h <- m_h - glm_rmse_high
l <- m_l + glm_rmse_low
glm_neg1 <- h-l
#with 2 rmse
#makes new intervals
glm_pred_low <- glm_pred_low %>% mutate(max = max + glm_rmse_low, min = min - glm_rmse_low)
glm_pred_high <- glm_pred_high %>% mutate(max = max + glm_rmse_high, min = min - glm_rmse_high)
#calculates accuracy
glm_accuracy_low2 <- mean(glm_pred_low$min <= test_set$low & glm_pred_low$max >= test_set$low)
glm_accuracy_high2 <- mean(glm_pred_high$min <= test_set$high & glm_pred_high$max >= test_set$high)
glm_total_accuracy2 <- mean(glm_pred_high$min <= test_set$high & glm_pred_high$max >= test_set$high & glm_pred_low$min <= test_set$low & glm_pred_low$max >= test_set$low)
glm_partial_accuracy2 <- mean((glm_pred_high$min <= test_set$high & glm_pred_high$max >= test_set$high) | (glm_pred_low$min <= test_set$low & glm_pred_low$max >= test_set$low))
#calculates trading margin
h <- m_h - 2 * glm_rmse_high
l <- m_l + 2 * glm_rmse_low
glm_neg2 <- h-l

#divides train_set into train_set2 and test_set2
test_index2 <- createDataPartition(y = train_set$high, times = 1, p = 0.1, list = FALSE)
train_set2 <- train_set[-test_index2,]
test_set2 <- train_set[test_index2,]

#Local Wheited
#choses span by RMSE
spans <- seq(0.17, 0.5, 0.03)
#for high
loess_high_accuracy <- sapply(spans, function(s){
  fit <- train(high ~ . -low -`high_-1` -`low_-1`, method = "gamLoess", tuneGrid = data.frame(span = s, degree = 1),  train_set2)
  #predicts and makes rmse at test set
  y_hat <- predict(fit, test_set2)
  test_error <- RMSE(test_set2$high, y_hat)
  test_error
})
#gathers the data
loess_high_accuracy <- data.frame(rmse = loess_high_accuracy, spans = spans)
#save(loess_high_accuracy, file = "rdas/loess-high-accuracy.rda")
#for low
loess_low_accuracy <- sapply(spans, function(s){
  fit <- train(low ~ . -high -`high_-1` -`low_-1`, method = "gamLoess", tuneGrid = data.frame(span = s, degree = 1),  train_set2)
  #predicts and makes rmse at test set
  y_hat <- predict(fit, test_set2)
  test_error <- RMSE(test_set2$low, y_hat)
  test_error
})
#gathers the data
loess_low_accuracy <- data.frame(rmse = loess_low_accuracy, spans = spans)
#save(loess_low_accuracy, file = "rdas/loess-low-accuracy.rda")
#best span
span_l <- with(loess_low_accuracy, spans[which.min(rmse)])
span_h <- with(loess_high_accuracy, spans[which.min(rmse)])
#runs loess with best results
fit_loess_high <- train(high ~ . -low -`high_-1` -`low_-1`, method = "gamLoess", tuneGrid = data.frame(span = span_h, degree = 1),  train_set)
fit_loess_low <- train(low ~ . -high -`high_-1` -`low_-1`, method = "gamLoess", tuneGrid = data.frame(span = span_l, degree = 1),  train_set)
#save(fit_loess_low, file = "rdas/fit-loess-low.rda")
#save(fit_loess_high, file = "rdas/fit-loess-high.rda")
#makes predictions
loess_pred_high <- predict(fit_loess_high, test_set)
loess_pred_low <- predict(fit_loess_low, test_set)
#with 1 rmse
#calculates margin of error
loess_rmse_low <- RMSE(loess_pred_low, test_set$low)
loess_rmse_high <- RMSE(loess_pred_high, test_set$high)
#makes intervals
loess_pred_low <- data.frame(pred = loess_pred_low, max = loess_pred_low + loess_rmse_low, min = loess_pred_low - loess_rmse_low)
loess_pred_high <- data.frame(pred = loess_pred_high, max = loess_pred_high + loess_rmse_high, min = loess_pred_high - loess_rmse_high)
#calculates accuracy
loess_accuracy_low1 <- mean(loess_pred_low$min <= test_set$low & loess_pred_low$max >= test_set$low)
loess_accuracy_high1 <- mean(loess_pred_high$min <= test_set$high & loess_pred_high$max >= test_set$high)
loess_total_accuracy1 <- mean(loess_pred_high$min <= test_set$high & loess_pred_high$max >= test_set$high & loess_pred_low$min <= test_set$low & loess_pred_low$max >= test_set$low)
loess_partial_accuracy1 <- mean((loess_pred_high$min <= test_set$high & loess_pred_high$max >= test_set$high) | (loess_pred_low$min <= test_set$low & loess_pred_low$max >= test_set$low))
#calculates trading margin
m_h <- mean(loess_pred_high$pred)
m_l <- mean(loess_pred_low$pred)
h <- m_h - loess_rmse_high
l <- m_l + loess_rmse_low
loess_neg1 <- h-l
#with 2 rmse
#makes new intervals
loess_pred_low <- loess_pred_low %>% mutate(max = max + loess_rmse_low, min = min - loess_rmse_low)
loess_pred_high <- loess_pred_high %>% mutate(max = max + loess_rmse_high, min = min - loess_rmse_high)
#calculates accuracy
loess_accuracy_low2 <- mean(loess_pred_low$min <= test_set$low & loess_pred_low$max >= test_set$low)
loess_accuracy_high2 <- mean(loess_pred_high$min <= test_set$high & loess_pred_high$max >= test_set$high)
loess_total_accuracy2 <- mean(loess_pred_high$min <= test_set$high & loess_pred_high$max >= test_set$high & loess_pred_low$min <= test_set$low & loess_pred_low$max >= test_set$low)
loess_partial_accuracy2 <- mean((loess_pred_high$min <= test_set$high & loess_pred_high$max >= test_set$high) | (loess_pred_low$min <= test_set$low & loess_pred_low$max >= test_set$low))
#calculates trading margin
h <- m_h - 2 * loess_rmse_high
l <- m_l + 2 * loess_rmse_low
loess_neg2 <- h-l

#K-nears-neighbors
#choses k size by RMSE
ks <- seq(3, 150, 3)
#for high
knn_high_accuracy <- sapply(ks, function(k){
  fit <- train(high ~ . -low, method = "knn", tuneGrid = data.frame(k = k), data = train_set2)
  #predicts and makes rmse at train set
  y_hat <- predict(fit, train_set2)
  train_error <- RMSE(train_set2$high, y_hat)
  #predicts and makes rmse at test set
  y_hat <- predict(fit, test_set2)
  test_error <- RMSE(test_set2$high, y_hat)
  #gathers the data
  tibble(train = train_error, test = test_error)
})
#gathers the data
knn_high_accuracy <- bind_rows(knn_high_accuracy %>% select(train) %>% mutate(k = ks, type = "train") %>% 
                             rename(rmse = train), knn_high_accuracy %>% select(test) %>% mutate(k = ks, type = "test") %>% 
                             rename(rmse = test))
#save(knn_high_accuracy, file = "rdas/knn-high-accuracy.rda")
#for low
knn_low_accuracy <- sapply(ks, function(k){
  fit <- train(low ~ . -high, method = "knn", tuneGrid = data.frame(k = k), data = train_set2)
  #predicts and makes rmse at train set
  y_hat <- predict(fit, train_set2)
  train_error <- RMSE(train_set2$low, y_hat)
  #predicts and makes rmse at test set
  y_hat <- predict(fit, test_set2)
  test_error <- RMSE(test_set2$low, y_hat)
  #gathers the data
  tibble(train = train_error, test = test_error)
})
#gathers the data
knn_low_accuracy <- bind_rows(knn_low_accuracy %>% select(train) %>% mutate(k = ks, type = "train") %>% 
                                 rename(rmse = train), knn_low_accuracy %>% select(test) %>% mutate(k = ks, type = "test") %>% 
                                 rename(rmse = test))
#save(knn_low_accuracy, file = "rdas/knn-low-accuracy.rda")
#best ks
k_l <- knn_low_accuracy %>% filter(type == "test")
k_l <- with(k_l, k[which.min(rmse)])
k_h <- knn_high_accuracy %>% filter(type == "test")
k_h <- with(k_h, k[which.min(rmse)])
#run knn with best results
fit_knn_high <- train(high ~ . -low, method = "knn", tuneGrid = data.frame(k = k_h), train_set)
fit_knn_low <- train(low ~ . -high, method = "knn", tuneGrid = data.frame(k = k_l), train_set)
#save(fit_knn_low, file = "rdas/fit-knn-low.rda")
#save(fit_knn_high, file = "rdas/fit-knn-high.rda")
#makes predictions
knn_pred_high <- predict(fit_knn_high, test_set)
knn_pred_low <- predict(fit_knn_low, test_set)
#with 1 rmse
#calculates margin of error
knn_rmse_low <- RMSE(knn_pred_low, test_set$low)
knn_rmse_high <- RMSE(knn_pred_high, test_set$high)
#makes intervals
knn_pred_low <- data.frame(pred = knn_pred_low, max = knn_pred_low + knn_rmse_low, min = knn_pred_low - knn_rmse_low)
knn_pred_high <- data.frame(pred = knn_pred_high, max = knn_pred_high + knn_rmse_high, min = knn_pred_high - knn_rmse_high)
#calculates accuracy
knn_accuracy_low1 <- mean(knn_pred_low$min <= test_set$low & knn_pred_low$max >= test_set$low)
knn_accuracy_high1 <- mean(knn_pred_high$min <= test_set$high & knn_pred_high$max >= test_set$high)
knn_total_accuracy1 <- mean(knn_pred_high$min <= test_set$high & knn_pred_high$max >= test_set$high & knn_pred_low$min <= test_set$low & knn_pred_low$max >= test_set$low)
knn_partial_accuracy1 <- mean((knn_pred_high$min <= test_set$high & knn_pred_high$max >= test_set$high) | (knn_pred_low$min <= test_set$low & knn_pred_low$max >= test_set$low))
#calculates trading margin
m_h <- mean(knn_pred_high$pred)
m_l <- mean(knn_pred_low$pred)
h <- m_h - knn_rmse_high
l <- m_l + knn_rmse_low
knn_neg1 <- h-l
#with 2 rmse
#makes new intervals
knn_pred_low <- knn_pred_low %>% mutate(max = max + knn_rmse_low, min = min - knn_rmse_low)
knn_pred_high <- knn_pred_high %>% mutate(max = max + knn_rmse_high, min = min - knn_rmse_high)
#calculates accuracy
knn_accuracy_low2 <- mean(knn_pred_low$min <= test_set$low & knn_pred_low$max >= test_set$low)
knn_accuracy_high2 <- mean(knn_pred_high$min <= test_set$high & knn_pred_high$max >= test_set$high)
knn_total_accuracy2 <- mean(knn_pred_high$min <= test_set$high & knn_pred_high$max >= test_set$high & knn_pred_low$min <= test_set$low & knn_pred_low$max >= test_set$low)
knn_partial_accuracy2 <- mean((knn_pred_high$min <= test_set$high & knn_pred_high$max >= test_set$high) | (knn_pred_low$min <= test_set$low & knn_pred_low$max >= test_set$low))
#calculates trading margin
h <- m_h - 2 * knn_rmse_high
l <- m_l + 2 * knn_rmse_low
knn_neg2 <- h-l

#Random Forest
#choses number of trees by RMSE
trees <- seq(2, 10, 1)
#for high
rf_high_accuracy <- sapply(trees, function(trees){
  fit <- train(high ~ . -low -`high_-1` -`low_-1`, method = "rf", tuneGrid = data.frame(mtry = trees),  train_set2)
  #predicts and makes rmse at test set
  y_hat <- predict(fit, test_set2)
  test_error <- RMSE(test_set2$high, y_hat)
  test_error
})
#gathers the data
rf_high_accuracy <- data.frame(rmse = rf_high_accuracy, trees = trees)
#save(rf_high_accuracy, file = "rdas/rf-high-accuracy.rda")
#for low
rf_low_accuracy <- sapply(trees, function(s){
  fit <- train(low ~ . -high -`high_-1` -`low_-1`, method = "rf", tuneGrid = data.frame(mtry = trees),  train_set2)
  #predicts and makes rmse at test set
  y_hat <- predict(fit, test_set2)
  test_error <- RMSE(test_set2$low, y_hat)
  test_error
})
#gathers the data
rf_low_accuracy <- data.frame(rmse = rf_low_accuracy, trees = trees)
#save(rf_low_accuracy, file = "rdas/rf-low-accuracy.rda")
#best mtry
mtry_l <- with(rf_low_accuracy, trees[which.min(rmse)])
mtry_h <- with(rf_high_accuracy, trees[which.min(rmse)])
#run rf with best results
fit_rf_high <- train(high ~ . -low, tuneGrid = data.frame(mtry = mtry_h), method = "rf", train_set)
fit_rf_low <- train(low ~ . -high, tuneGrid = data.frame(mtry = mtry_l), method = "rf", train_set)
#save(fit_rf_low, file = "rdas/fit-rf-low.rda")
#save(fit_rf_high, file = "rdas/fit-rf-high.rda")
#makes predictions
rf_pred_high <- predict(fit_rf_high, test_set)
rf_pred_low <- predict(fit_rf_low, test_set)
#with 1 rmse
#calculates margin of error
rf_rmse_low <- RMSE(rf_pred_low, test_set$low)
rf_rmse_high <- RMSE(rf_pred_high, test_set$high)
#makes intervals
rf_pred_low <- data.frame(pred = rf_pred_low, max = rf_pred_low + rf_rmse_low, min = rf_pred_low - rf_rmse_low)
rf_pred_high <- data.frame(pred = rf_pred_high, max = rf_pred_high + rf_rmse_high, min = rf_pred_high - rf_rmse_high)
#calculates accuracy
rf_accuracy_low1 <- mean(rf_pred_low$min <= test_set$low & rf_pred_low$max >= test_set$low)
rf_accuracy_high1 <- mean(rf_pred_high$min <= test_set$high & rf_pred_high$max >= test_set$high)
rf_total_accuracy1 <- mean(rf_pred_high$min <= test_set$high & rf_pred_high$max >= test_set$high & rf_pred_low$min <= test_set$low & rf_pred_low$max >= test_set$low)
rf_partial_accuracy1 <- mean((rf_pred_high$min <= test_set$high & rf_pred_high$max >= test_set$high) | (rf_pred_low$min <= test_set$low & rf_pred_low$max >= test_set$low))
#calculates trading margin
m_h <- mean(rf_pred_high$pred)
m_l <- mean(rf_pred_low$pred)
h <- m_h - rf_rmse_high
l <- m_l + rf_rmse_low
rf_neg1 <- h-l
#with 2 rmse
#makes new intervals
rf_pred_low <- rf_pred_low %>% mutate(max = max + rf_rmse_low, min = min - rf_rmse_low)
rf_pred_high <- rf_pred_high %>% mutate(max = max + rf_rmse_high, min = min - rf_rmse_high)
#calculates accuracy
rf_accuracy_low2 <- mean(rf_pred_low$min <= test_set$low & rf_pred_low$max >= test_set$low)
rf_accuracy_high2 <- mean(rf_pred_high$min <= test_set$high & rf_pred_high$max >= test_set$high)
rf_total_accuracy2 <- mean(rf_pred_high$min <= test_set$high & rf_pred_high$max >= test_set$high & rf_pred_low$min <= test_set$low & rf_pred_low$max >= test_set$low)
rf_partial_accuracy2 <- mean((rf_pred_high$min <= test_set$high & rf_pred_high$max >= test_set$high) | (rf_pred_low$min <= test_set$low & rf_pred_low$max >= test_set$low))
#calculates trading margin
h <- m_h - 2 * rf_rmse_high
l <- m_l + 2 * rf_rmse_low
rf_neg2 <- h-l

#makes results table
#with 1 rmse
results1 <- data.frame(names = c("Logistic", "Loess", "Knn", "RF"), 
  `Low Accuracy` = c(glm_accuracy_low1, loess_accuracy_low1, knn_accuracy_low1, rf_accuracy_low1), 
  `High Accuracy` = c(glm_accuracy_high1, loess_accuracy_high1, knn_accuracy_high1, rf_accuracy_high1), 
  `Total Accuracy` = c(glm_total_accuracy1, loess_total_accuracy1, knn_total_accuracy1, rf_total_accuracy1), 
  `Partial Accuracy` = c(glm_partial_accuracy1, loess_partial_accuracy1, knn_partial_accuracy1, rf_partial_accuracy1),
  `Trading Margin` = c(glm_neg1, loess_neg1, knn_neg1, rf_neg1),
  rmse_low = c(glm_rmse_low, loess_rmse_low, knn_rmse_low, rf_rmse_low), 
  rms_high =c(glm_rmse_high, loess_rmse_high, knn_rmse_high, rf_rmse_high))

#with 2 rmse
results2 <- data.frame(names = c("Logistic", "Loess", "Knn", "RF"), 
  `Low Accuracy` = c(glm_accuracy_low2, loess_accuracy_low2, knn_accuracy_low2, rf_accuracy_low2), 
  `High Accuracy` = c(glm_accuracy_high2, loess_accuracy_high2, knn_accuracy_high2, rf_accuracy_high2), 
  `Total Accuracy` = c(glm_total_accuracy2, loess_total_accuracy2, knn_total_accuracy2, rf_total_accuracy2), 
  `Partial Accuracy` = c(glm_partial_accuracy2, loess_partial_accuracy2, knn_partial_accuracy2, rf_partial_accuracy2),
  `Trading Margin` = c(glm_neg2, loess_neg2, knn_neg2, rf_neg2))

results1 <- results1 %>% mutate(Low.Accuracy = round(Low.Accuracy*100,2), High.Accuracy = round(High.Accuracy*100,2),
                                Total.Accuracy = round(Total.Accuracy*100,2), Partial.Accuracy = round(Partial.Accuracy*100,2),
                                Trading.Margin = round(Trading.Margin,2))
results2 <- results2 %>% mutate(Low.Accuracy = round(Low.Accuracy*100,2), High.Accuracy = round(High.Accuracy*100,2),
                                Total.Accuracy = round(Total.Accuracy*100,2), Partial.Accuracy = round(Partial.Accuracy*100,2),
                                Trading.Margin = round(Trading.Margin,2))
#save(results1, file = "rdas/results1.rda")
#save(results2, file = "rdas/results2.rda")


