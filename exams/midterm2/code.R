options(java.parameters = "-Xmx8000m")
pacman::p_load(YARF, tidyverse)
pacman::p_load_gh("coatless/ucidata")

##############
#### Problem 1
##############

rm(list = ls())
data(adult)
adult = na.omit(adult) #kill any observations with missingness

set.seed(1)
train_size = 5000
train_indices = sample(1 : nrow(adult), train_size)
adult_train = adult[train_indices, ]
y_train = adult_train$income
X_train = adult_train
X_train$income = NULL

test_size = 5000
test_indices = sample(setdiff(1 : nrow(adult), train_indices), test_size)
adult_test = adult[test_indices, ]
y_test = as.numeric(adult_test$income) -1
X_test = adult_test
X_test$income = NULL

logistic_mod = glm(income ~ ., adult_train, family = "binomial")
p_hats_train = predict(logistic_mod, adult_train, type = "response")
p_hats_test = predict(logistic_mod, adult_test, type = "response")

oos_conf_tab = table(y_test, y_test_hat = factor(ifelse(p_hats_test >= 0.2, 1, 0)))
oos_conf_tab / rbind(colSums(oos_conf_tab), colSums(oos_conf_tab))
oos_conf_tab = table(y_test, y_test_hat = factor(ifelse(p_hats_test >= 0.4, 1, 0)))
oos_conf_tab / rbind(colSums(oos_conf_tab), colSums(oos_conf_tab))
oos_conf_tab = table(y_test, y_test_hat = factor(ifelse(p_hats_test >= 0.6, 1, 0)))
oos_conf_tab / rbind(colSums(oos_conf_tab), colSums(oos_conf_tab))
oos_conf_tab = table(y_test, y_test_hat = factor(ifelse(p_hats_test >= 0.95, 1, 0)))
oos_conf_tab / rbind(colSums(oos_conf_tab), colSums(oos_conf_tab))

##############
#### Problem 4
##############

rm(list = ls())
mean(MASS::Boston$medv)
Dtrain_1_1 = MASS::Boston[1:300, ]
Dselect_1_1 = MASS::Boston[301:400, ]
mod_1_1_0 = lm(medv ~ 1, Dtrain_1_1)
mod_1_1_M = lm(medv ~ . * ., Dtrain_1_1)
round(summary(mod_1_1_0)$sigma, 2)
round(summary(mod_1_1_M)$sigma, 2)
yhat_1_1_0 = predict(mod_1_1_0, Dselect_1_1)
sqrt(mean((yhat_1_1_0 - Dselect_1_1$medv)^2))
yhat_1_1_M = predict(mod_1_1_M, Dselect_1_1)
sqrt(mean((yhat_1_1_M - Dselect_1_1$medv)^2))

tree_mod = YARFCART(MASS::Boston %>% select(-medv), MASS::Boston$medv, nodesize = 200)
illustrate_trees(tree_mod, open_file = TRUE)


