options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)



x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")


########################Q6

pca <- prcomp(x_scaled)
summary(pca)



########################Q7


data.frame(pca$x[,1:2], result=brca$y) %>% 
  ggplot(aes(PC1,PC2, fill = result))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)


########################Q8

for(i in 1:10){
  boxplot(pca$x[,i] ~ brca$y, main = paste("PC", i))
}


data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()


########################Q9

