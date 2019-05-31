rm(list = ls())

library(readr)
sn_diana <- read_csv("FComputacional/Estadistica/sn_diana.csv")

# Setting seed for replicability
set.seed(12345)
library(MASS)
library(mmc)
# Generating main dataset with 1000 observations
n <- 1000
# We generate data assuming that one predictor is measured with error
# Setting parameters for between person covariance matrix
var_between <- var(sn_diana$m15B, y=NULL)
# Setting parameters for within person covariance matrix
var_within <- 0
var_total <- var_within + var_between

# Generating data with a random variable
data_truth <- sn_diana$m15B
# Generating measurement error for the random variable
measurement_error <- sn_diana$Em15B
# The 'observed' data is constructed by adding the measurement error to the generated data
data_observed <- data_truth + measurement_error
x1 = data_observed
y= sn_diana$i

# Set up a main dataset with the outcome variable and a predictor,
# measured with error.
datalin <- data.frame(y = y, x1=x1)

# Fitting a linear regression
glmfit.lin1 <- glm(y~data_truth, family="gaussian", data=datalin)
summary(glmfit.lin1)
()

