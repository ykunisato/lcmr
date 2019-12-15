rm(list=ls())
library(pracma)
US  <- c(1,0,0,1,1,0,1,0,1,1,0,0,1,0,0,1,1,0, 0,0,0,0,0,0,0,0,0,0,0,0)
CS1 <- c(1,0,0,1,1,0,1,0,1,1,0,0,1,0,0,1,1,0, 1,1,0,1,0,0,1,0,1,1,0,0)
CS2 <- c(0,1,1,0,0,1,0,1,0,0,1,1,0,1,1,0,0,1, 0,0,1,0,1,1,0,1,0,0,1,1)

time <- c(0, seq(1:17)*4, 100+seq(1:12)*4)

X <- cbind(US,CS1,CS2)




source('R/associative_structure_learning.R')
learn_associative_structure(X,time)
