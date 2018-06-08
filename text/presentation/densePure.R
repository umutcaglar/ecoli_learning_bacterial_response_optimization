# explain dense and pure

###*****************************
# INITIAL COMMANDS TO RESET THE SYSTEM
rm(list = ls())
if (is.integer(dev.list())){dev.off()}
cat("\014")
###*****************************


###*****************************
# Required 
library(tidyverse)
library(mvtnorm)
library("cowplot")
###*****************************

 
###*****************************
# Data sets
AA <- data.frame(rmvnorm(30, mean = c(5, 3), sigma = matrix(c(.7, 0, 0, .7), nrow = 2)), class = "Densest")
BB <- data.frame(rmvnorm(60, mean = c(4, 7), sigma = matrix(c(8, 0, 0, 8), nrow = 2)), class = "Sparsest")
CC <- data.frame(rmvnorm(30, mean = c(17, 6), sigma = matrix(c(3, 0, 0, 3), nrow = 2)), class = "Purest")

DF <- rbind(AA,BB,CC)
###*****************************


###*****************************
# Figure
fig01 <- ggplot2::ggplot(data = DF, aes(x=X1, y= X2, colour=class ))+
  geom_point(size = 2, alpha = 0.7)+
  theme_bw()+
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3") )+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

fig01

cowplot::save_plot(filename = "densePure.jpeg", plot = fig01, ncol= 1.33)
###*****************************