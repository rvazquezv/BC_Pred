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