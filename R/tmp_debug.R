rm(list=ls())
library(pracma)
library(tidyverse)
library(gridExtra)
library(stringr)

# Experimental condition
US  <- c(1,0,0,1,1,0,1,0,1,1,0,0,1,0,0,1,1,0, 0,0,0,0,0,0,0,0,0,0,0,0, 0)
CS1 <- c(1,0,0,1,1,0,1,0,1,1,0,0,1,0,0,1,1,0, 1,1,0,1,0,0,1,0,1,1,0,0, 1)
CS2 <- c(0,1,1,0,0,1,0,1,0,0,1,1,0,1,1,0,0,1, 0,0,1,0,1,1,0,1,0,0,1,1, 0)
#Context <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 0,0,0,0,0,0,0,0,0,0,0,0, 1)
time <- c(0, seq(1:17)*4, 100+seq(1:12)*4, 86400)

#US  <- c(1,0,0,1,1,0,1,0,1,1,0,0,1,0,0,1,1,0, 0,0,0,0,0,0,0,0,0,0,0,0)
#CS1 <- c(1,0,0,1,1,0,1,0,1,1,0,0,1,0,0,1,1,0, 1,1,0,1,0,0,1,0,1,1,0,0)
#CS2 <- c(0,1,1,0,0,1,0,1,0,0,1,1,0,1,1,0,0,1, 0,0,1,0,1,1,0,1,0,0,1,1)
#time <- c(0, seq(1:17)*4, 100+seq(1:12)*4)

n_cs1 <- sum(CS1)
n_cs2 <- sum(CS2)
X <- cbind(US,CS1,CS2)
#X <- cbind(US,CS1,CS2,Context)

source('R/associative_structure_learning.R')
results <- learn_associative_structure(X,time,asl_opts<-list(c_alpha = 0.2, nst=0))

# Plot results(V&Z)
index_z <- as.data.frame(which(results$results$Z == 1, arr.ind = TRUE))
data <- data.frame(CS = CS1,
                   US=US,
                   V=results$results$V,
                   Z=index_z$col)

# Plot results(prob cause before US)
data_p <- as.data.frame(results$results$p) %>%
  mutate(CS1=CS1) %>%
  tidyr::gather(key="Cause",value="probability",-CS1)

data_p$Cause <- str_replace(data_p$Cause,pattern="V", replacement="C")
data_p$Cause <- factor(data_p$Cause,levels = c("C1","C2","C3","C4","C5","C6","C7","C8",
                                               "C9","C10","C11","C12","C13","C14","C15"))

# Plot results(prob cause After US)
data_zp <- as.data.frame(results$results$Zp) %>%
  mutate(CS1=CS1) %>%
  tidyr::gather(key="Cause",value="probability",-CS1)

data_zp$Cause <- str_replace(data_zp$Cause,pattern="V", replacement="C")
data_zp$Cause <- factor(data_zp$Cause,levels = c("C1","C2","C3","C4","C5","C6","C7","C8",
                                               "C9","C10","C11","C12","C13","C14","C15"))


p1 <- data %>%
  filter(CS1==1) %>%
  mutate(trial=1:n_cs1) %>%
  ggplot(aes(x=trial, y=V)) +
  geom_line() +
  geom_point(aes(x=trial,y=US)) +
  labs(title="CS+",y="CR")+
  ylim(0,1)


p2 <- data %>%
  filter(CS1==0) %>%
  mutate(trial=1:n_cs2) %>%
  ggplot(aes(x=trial, y=V)) +
  geom_line() +
  geom_point(aes(x=trial,y=US)) +
  labs(title="CS-",y="CR")+
  ylim(0,1)

p3 <- data %>%
  filter(CS1==1) %>%
  mutate(trial=1:n_cs1) %>%
  ggplot(aes(x=trial, y=Z)) +
  geom_line() +
  labs(y="Latent cause")+
  ylim(0,15)

p4 <- data %>%
  filter(CS1==0) %>%
  mutate(trial=1:n_cs2) %>%
  ggplot(aes(x=trial, y=Z)) +
  geom_line() +
  labs(y="Latent cause")+
  ylim(0,15)

p5 <- data_p %>%
  filter(CS1==1) %>%
  mutate(trial=rep(1:n_cs1,times=15)) %>%
  ggplot(aes(x=trial, y=probability, color = Cause)) +
  geom_line() +
  labs(title="CS+ before US",y="Probability")+
  ylim(0,1)

p6 <- data_p %>%
  filter(CS1==0) %>%
  mutate(trial=rep(1:n_cs2,times=15)) %>%
  ggplot(aes(x=trial, y=probability, color = Cause)) +
  geom_line() +
  labs(title="CS- before US",y="Probability")+
  ylim(0,1)

p7 <- data_zp %>%
  filter(CS1==1) %>%
  mutate(trial=rep(1:n_cs1,times=15)) %>%
  ggplot(aes(x=trial, y=probability, color = Cause)) +
  geom_line() +
  labs(title="CS+ after US",y="Probability")+
  ylim(0,1)

p8 <- data_zp %>%
  filter(CS1==0) %>%
  mutate(trial=rep(1:n_cs2,times=15)) %>%
  ggplot(aes(x=trial, y=probability, color = Cause)) +
  geom_line() +
  labs(title="CS- after US",y="Probability")+
  ylim(0,1)

grid.arrange(p1, p2,p3,p4,ncol=2)

#grid.arrange(p5,p6,p7,p8,ncol=2)
