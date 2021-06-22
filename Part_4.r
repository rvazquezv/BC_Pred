########################Q14 https://rdrr.io/snippets/  

set.seed(7, sample.kind = "Rounding") 
grid<-data.frame(k = seq(3, 21, 2))
train_knn <- train(train_y ~ . , method = "knn", data = train_Set,tuneGrid = grid)
train_knn$bestTune

knn_prediction <- predict(train_knn, test_x)
confusionMatrix(knn_prediction, test_y)$overall["Accuracy"]

########################Q15

library(randomForest)
set.seed(9, sample.kind = "Rounding") 


train_RF <- train(train_y ~ ., method = "rf", data = train_Set,ntree=100, tuneGrid = data.frame(mtry = c(3, 5, 7, 9)),importance=TRUE)

train_RF$bestTune

y_hat_RF<- predict(train_RF, test_x)

mean(y_hat_RF == test_y)
confusionMatrix(y_hat_RF, test_y)$overall["Accuracy"]

varImp(train_RF)

########################Q16

models<-c("km_model_fin","y_hat_glm","lda_prediction","qda_prediction","loess_prediction","knn_prediction","y_hat_RF")

models_df=data.frame(factor(km_model_fin),y_hat_glm,lda_prediction,qda_prediction,loess_prediction,knn_prediction,y_hat_RF)


ensamble<-function(i) {
  ifelse(length(which(models_df[i,]=="M"))>3,"M","B")
}

ensfit<-sapply(seq(1:115),ensamble)

confusionMatrix(factor(ensfit),test_y)$overall["Accuracy"]


Accur<-function(i) {
  confusionMatrix(factor(models_df[,i]),test_y)$overall["Accuracy"]
}


data.frame(Model = models, Accuracy = sapply(1:7, Accur))

