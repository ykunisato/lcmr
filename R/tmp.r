setwd(paste0(getwd(),"/R"))
# function already made
source("LCM_opts.r")

# test stimulus set
X <- matrix(c(
  0,1,0,
  0,0,1,
  1,1,0,
  1,1,0,
  0,0,1,
  0,1,0,
  1,1,0,
  1,1,0,
  0,0,1,
  0,1,0,
  1,1,0,
  0,0,1,
  1,1,0,
  1,1,0,
  0,0,1,
  0,0,1
  ),
  ncol = 3,byrow=TRUE
)

# check function

