# load libraries
library(glmnetUtils)
library(tidyverse)
library(rpart)         # to train decision trees
library(rpart.plot)    # to plot decision trees
library(randomForest)  # random forests
library(gbm)           # boosting
library(tidyverse)     # tidyverse
library(kableExtra)    # for printing tables
library(cowplot)       # for side-by-side plots

# load test data
test_data = read_csv("data/clean/test_data.csv")
test_data = test_data %>% mutate(leading_party = as.numeric(leading_party == "Democrat")) %>%
  select(-state, -county, -fips, -total_votes, -Democrat, -Other, -Republican, -Green, -Libertarian, -pct_dem, -pct_rep, -pct_other, -pct_green, -pct_libertarian)
test_data$urban_rural_desc = as.factor(test_data$urban_rural_desc)

# load glm fit object
load("results/glm_fit.Rda")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

# evaluate glm RMSE
glm_fitted_probabilities = predict(glm_fit, newdata = test_data, type = "response")
glm_predictions = as.numeric(glm_fitted_probabilities > 0.5)
glm_misclassification = mean(glm_predictions != test_data$leading_party)

# evaluate ridge RMSE
ridge_fitted_probabilities = predict(ridge_fit,
                                     newdata = test_data,
                                     s = "lambda.1se",
                                     type = "response")
ridge_predictions = as.numeric(ridge_fitted_probabilities > 0.5)
ridge_misclassification = mean(ridge_predictions != test_data$leading_party)

# evaluate lasso RMSE
lasso_fitted_probabilities = predict(lasso_fit, 
                                     newdata = test_data, 
                                     s = "lambda.1se",
                                     type = "response")
lasso_predictions = as.numeric(lasso_fitted_probabilities > 0.5)
lasso_misclassification = mean(lasso_predictions != test_data$leading_party)

###Data preparation
##Read in data
test = read_csv("data/clean/test_data.csv")
#Make binary respones (1=Dem, 0=Rep)
test = test %>% 
  mutate(leading_party = as.numeric(leading_party == "Democrat"))

##Load fit objects
#Decision tree
load("results/dtr.Rda")
#Random forest
load("results/rf_fit.Rda")
#Boosting
load("results/gbm_fit.Rda")

###Test set evaluation and comparison
##Misclassification errors
#decision tree
pred_dtr = predict(optimal_tree, newdata = test_data)
dtr_mis_err = mean(pred_dtr != test_data$leading_party)
#random forest
pred_rf = predict(rf_fit3, newdata = test_data)
rf_mis_err = mean(pred_rf != test_data$leading_party)
#boosting
load("results/optimal_num_trees")
pred_gbm = predict(gbm_fit_optimal, 
                   newdata = test_data,
                   n.trees = optimal_num_trees,
                   type = "response")
gbm_mis_err = mean(as.numeric(pred_gbm > 0.5) != test$leading_party)

# print nice table
misclass_errs = tribble(
  ~Method, ~`Test Misclassification Errors`,
  "Logistic", glm_misclassification,
  "Ridge", ridge_misclassification,
  "Lasso", lasso_misclassification,
  "Decision Tree", dtr_mis_err,
  "Random Forest", rf_mis_err,
  "Boosting", gbm_mis_err
)
misclass_errs %>% write_csv("results/model-evaluation.csv")

rm(list=ls())