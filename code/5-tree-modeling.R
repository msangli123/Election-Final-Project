###Load appropriate packages
library(rpart)         # to train decision trees
library(rpart.plot)    # to plot decision trees
library(randomForest)  # random forests
library(gbm)           # boosting
library(tidyverse)     # tidyverse
library(kableExtra)    # for printing tables
library(cowplot)       # for side-by-side plots

###Data Preparation
##Read in data
train = read_csv("data/clean/train_data.csv")
train = train %>% 
  mutate(leading_party = as.numeric(leading_party == "Democrat")) %>% 
  select(-state, -county, -fips, -total_votes, -Democrat, -Other, -Republican, -Green, -Libertarian, -pct_dem, -pct_rep, -pct_other, -pct_green, -pct_libertarian)
train$urban_rural_desc = as.factor(train$urban_rural_desc)

###Decision Tree
##Growing the default tree
#fit
tree_fit = rpart(leading_party ~ .,
                 method = 'class',
                 parms = list(split = 'gini'),
                 data = train)

#plot
rpart.plot(tree_fit)

##Deepest possible tree
#fit
T_0 = rpart(leading_party ~ .,
            method = 'class',
            parms = list(split = 'gini'),
            control = rpart.control(minsplit = 1,
                                    minbucket = 1,
                                    cp = 0),
            data = train)
#cp table
cp_table = printcp(T_0) %>% 
  as_tibble()

##Pruning and cross-validation
#cv plot
cv_plot = cp_table %>%
  filter(nsplit >= 2) %>%
  ggplot(aes(x = nsplit+1, y = xerror,
             ymin = xerror - xstd, ymax = xerror + xstd)) +
  geom_point() + geom_line() +
  geom_errorbar(width = 0.2) +
  xlab("log10(Number of terminal nodes)") + ylab("CV error") +
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") +
  scale_x_log10()
theme_bw()
#optimal tree
set.seed(471) 
optimal_tree_info = cp_table %>%
  filter(xerror - xstd < min(xerror)) %>%
  arrange(nsplit) %>%
  head(1)
optimal_tree_info
optimal_tree = prune(tree = T_0, cp = optimal_tree_info$CP)
#Save optimal tree
save(optimal_tree, file = "results/dtr.Rda")
optimal_tree_plot = rpart.plot(optimal_tree)

###Random Forest
##Default random forest
#fit
set.seed(471) # for reproducibility (DO NOT CHANGE)
rf_fit = randomForest(factor(leading_party) ~ ., data = train)
#OOB error
def_OOB_plot = tibble(oob_error = rf_fit$err.rate[,"OOB"],trees = 1:500) %>%
  ggplot(aes(x = trees, y = oob_error)) + geom_line() + theme_bw()
ggsave(filename = "results/def_OOB_plot.png", 
       plot = def_OOB_plot, 
       device = "png", 
       width = 6, 
       height = 4)

##Tuning the random forest
#identify best value of m
set.seed(471) # for reproducibility (DO NOT CHANGE)

#test out 5 different ms
poss_m = seq(1,57,length.out=5)
vec.OOB_err = c() #empty container to store OOB error values

for(i in poss_m){
  rf_fit2 = randomForest(factor(leading_party) ~ ., 
                         mtry = i, 
                         ntrees = 300,
                         data = train)
  OOB_err = rf_fit2$err.rate[,"OOB"][300]
  vec.OOB_err = c(vec.OOB_err,OOB_err)
}
#create tibble
m_OOB_err = tibble(m = poss_m,
                   OOB_err = round(vec.OOB_err,4))
#plot the data
m_OOB_err_plot = m_OOB_err %>%
  ggplot(aes(x = m, y = OOB_err)) +
  geom_point() + geom_text(aes(label=m,hjust=2, vjust=1)) +
  xlab("Value of m") + ylab("OOB Error") +
  theme_bw()
ggsave(filename = "results/m_OOB_err_plot.png", 
        plot = m_OOB_err_plot, 
        device = "png", 
        width = 6, 
        height = 4)
#tune using optimal m
set.seed(471) # for reproducibility (DO NOT CHANGE)
rf_fit3 = randomForest(factor(leading_party) ~ .,
                       mtry = 29,
                       importance = TRUE,
                       data = train)
#OOB error plot w/ optimal m
optimal_OOB_err_plot = tibble(oob_error = rf_fit3$err.rate[,"OOB"],trees = 1:500) %>%
  ggplot(aes(x = trees, y = oob_error)) + geom_line() + theme_bw() +
  xlab("Number of Trees") + ylab("OOB Error")
#Save optimal fit
save(rf_fit3, file = "results/rf_fit.Rda")

###Boosting
##Fit boosted tree models with interaction depths 1, 2, and 3
set.seed(471) # for reproducibility (DO NOT CHANGE)
#interaction depth 1
gbm_fit1 = gbm(leading_party ~ .,
               distribution = "bernoulli",
               n.trees = 1000,
               interaction.depth = 1,
               shrinkage = 0.1,
               cv.folds = 5,
               data = train)

set.seed(471) # for reproducibility (DO NOT CHANGE)
#interaction depth 2
gbm_fit2 = gbm(leading_party ~ .,
               distribution = "bernoulli",
               n.trees = 1000,
               interaction.depth = 2,
               shrinkage = 0.1,
               cv.folds = 5,
               data = train)

set.seed(471) # for reproducibility (DO NOT CHANGE)
#interaction depth 3
gbm_fit3 = gbm(leading_party ~ .,
               distribution = "bernoulli",
               n.trees = 1000,
               interaction.depth = 3,
               shrinkage = 0.1,
               cv.folds = 5,
               data = train)
##Optimal fit
#CV Errors
ntrees = 1000

CV_errors = tibble(Iteration = 1:ntrees, CV_1 = gbm_fit1$cv.error,
                   CV_2 = gbm_fit2$cv.error, CV_3 = gbm_fit3$cv.error) 

gbm_CV_errs = CV_errors %>%
  ggplot() + 
  geom_line(aes(x = Iteration, y = CV_1, color = 'depth 1')) +
  geom_hline(yintercept=min(CV_errors$CV_1),linetype = 'dashed') +
  
  geom_line(aes(x = Iteration, y = CV_2, color = 'depth 2')) +
  geom_hline(yintercept=min(CV_errors$CV_2),linetype = 'dashed') +
  
  geom_line(aes(x = Iteration, y = CV_3, color = 'depth 3')) + 
  geom_hline(yintercept=min(CV_errors$CV_3),linetype = 'dashed') +
  xlab("Number of Trees") + ylab("CV error") +
  theme_bw()

ggsave('results/gbm_CV_errs.png',
       plot = gbm_CV_errs, 
       device = "png", 
       width = 6, 
       height = 4)
#Optimal number of trees
gbm_fit_optimal = gbm_fit3
optimal_num_trees = gbm.perf(gbm_fit3, plot.it = FALSE)

##Need to save optimal num trees
optimal_num_trees
save(optimal_num_trees, file = "results/optimal_num_trees")
#Save optimal fit
save(gbm_fit_optimal, file = "results/gbm_fit.Rda")

###Interpretation
##Random forest
#Variable importance
var_imp = varImpPlot(rf_fit3,n.var=10)

##Boosting
#Relative Influence
rel_inf = summary(gbm_fit_optimal, n.trees = optimal_num_trees, plotit = FALSE)[1:10,] %>%
  as_tibble() %>% write_tsv("results/gbm-rel-inf.tsv")

#Partial dependence plots
#p1
p1 = plot(gbm_fit_optimal, i.var = "log_traffic_volume", n.trees = optimal_num_trees)

#p2
p2 = plot(gbm_fit_optimal, i.var = "log_poverty_rating", n.trees = optimal_num_trees)

#p3
p3 = plot(gbm_fit_optimal, i.var = "severe_housing_issues", n.trees = optimal_num_trees)

part_dep_plots = plot_grid(p1,p2,p3)
ggsave('results/part-dep-plots.png',
       plot = part_dep_plots, 
       device = "png", 
       width = 6, 
       height = 4)

rm(list=ls())