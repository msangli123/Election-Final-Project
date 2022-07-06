library(tidyverse)

# read in the cleaned data
master_data = read_csv("data/clean/master_data.csv")
master_data$urban_rural_desc = as.factor(master_data$urban_rural_desc)

# split into train and test (set seed here if applicable)
# seed set for reproducibility (DO NOT CHANGE)
set.seed(471) 
n = nrow(master_data)
train_samples = sample(1:n, round(0.8*n))
train_data = master_data[train_samples,]
test_data = master_data[-train_samples,]

# save the train and test data
write_csv(x = train_data, file = "data/clean/train_data.csv")
write_csv(x = test_data, file = "data/clean/test_data.csv")

rm(list=ls())