rm(list = ls())

######### Dependencies
library(pracma)

# function already made
source("LCM_opts.r")
source("LCM_infer.r")
source("LCM_lik.r")

# test dataset
data <- read.csv("test_data.csv")
