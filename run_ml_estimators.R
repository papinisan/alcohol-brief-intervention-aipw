# this code uses the model_avs.R function to run all machine learning analyses
library(h2o)
source("model_avs.R")
# load preprocess and features
# treatment model
model_avs(preprocess, features, "BI", c("1", "0"), "pi")
# follow-up participation model
model_avs(preprocess, features, "uncensored", c("1", "0"), "delta")
# outcome models
model_avs(preprocess, features, "uncensored", c("1", "0"), "delta")
## change in drinking days
model_avs(preprocess, features, "drinking_days_delta", "1", "mu1_drinking_days_delta")
model_avs(preprocess, features, "drinking_days_delta", "0", "mu0_drinking_days_delta")
## change in heavy drinking days
model_avs(preprocess, features, "binge_days_delta", "1", "mu1_binge_days_delta")
model_avs(preprocess, features, "binge_days_delta", "0", "mu0_binge_days_delta")
## change in drinks per drinking day
model_avs(preprocess, features, "drinks_day_delta", "1", "mu1_drinks_day_delta")
model_avs(preprocess, features, "drinks_day_delta", "0", "mu0_drinks_day_delta")
## change in drinks per week
model_avs(preprocess, features, "drinks_week_delta", "1", "mu1_drinks_week_delta")
model_avs(preprocess, features, "drinks_week_delta", "0", "mu0_drinks_week_delta")