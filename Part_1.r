options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

########################Q1

dim(brca$x)


length(which(brca$y=="M"))/length(brca$y)

which.max(colMeans(brca$x))


which.min(colSds(brca$x))



########################Q2


x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

sd(x_scaled[,1])
colSds(x_scaled)

colMedians(x_scaled)
median(x_scaled[,1])


########################Q3

d<-dist(x_scaled)
idx_B<-which(brca$y=="B")




