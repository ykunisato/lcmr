rm(list = ls())

######### Dependencies
library(pracma)

setwd(paste0(getwd(),"/R"))
# function already made
source("LCM_opts.r")
source("LCM_infer.r")

# test stimulus set
data <- list()

data$US <- matrix(c(
  0,
  0,
  1,
  1,
  0,
  0,
  1,
  1,
  0,
  0,
  1,
  0,
  1,
  1,
  0,
  0
),
ncol = 1,byrow=TRUE
)

data$CS <- matrix(c(
  1,0,
  0,1,
  1,0,
  1,0,
  0,1,
  1,0,
  1,0,
  1,0,
  0,1,
  1,0,
  1,0,
  0,1,
  1,0,
  1,0,
  0,1,
  0,1
  ),
  ncol = 2,byrow=TRUE
)

data$CR <- matrix(c(
  0.1,
  0.1,
  0.8,
  0.9,
  0.2,
  0.2,
  1.0,
  1.1,
  0.1,
  0.05,
  1.4,
  0.2,
  1.5,
  1.6,
  0,
  0
),
ncol = 1,byrow=TRUE
)

# change paramerter name from alpha to c_alpha
alpha <- 0.5






