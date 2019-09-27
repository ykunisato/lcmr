library(pracma)

# function already made
source("LCM_opts.r")
source("LCM_infer.r")
source("LCM_lik.r")


data <- read.csv("test_data.csv")
results <- LCM_fit(data,2)
