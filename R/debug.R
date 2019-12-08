rm(list=ls())

# Test condition
US  <- c(1,0,0,1,1,0,1,0,1,1,0,0,1,0,0,1,1,0, 0,0,0,0,0,0,0,0,0,0,0,0)
CS1 <- c(1,0,0,1,1,0,1,0,1,1,0,0,1,0,0,1,1,0, 1,1,0,1,0,0,1,0,1,1,0,0)
CS2 <- c(0,1,1,0,0,1,0,1,0,0,1,1,0,1,1,0,0,1, 0,0,1,0,1,1,0,1,0,0,1,1)
trials<-c(1,1,2,2,3,3,4,4,5,6,5,6,7,7,8,8,9,9, 10,11,10,12,11,12,13,13,14,15,14,15)
X<-cbind(US,CS1,CS2)

library(pracma)
library(tidyverse)
library(gridExtra)

source("R/infer_latent_cause.R")
source("R/set_lcm_opts.R")
opts <- list(c_alpha=0.1,K=10,stickiness=0)
results <- infer_latent_cause(X,opts)

check_data <- data.frame(trials,US,CS1,CS2,V=results[["V"]])
cs_1 <- check_data %>%
  filter(CS1==1) %>%
  ggplot(aes(x=trials,y=V))+
  geom_line() +
  geom_point(aes(x=trials,y=US)) +
  ylim(0,1)+
  labs(title="CS+")

cs_2 <- check_data %>%
  filter(CS2==1) %>%
  ggplot(aes(x=trials,y=V))+
  geom_line() +
  geom_point(aes(x=trials,y=US)) +
  ylim(0,1)+
  labs(title="CS-")

gridExtra::grid.arrange(cs_1, cs_2, nrow = 1)
