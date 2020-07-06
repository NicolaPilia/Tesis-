tfidf_2<-tfidf
kfold_LSA<-function(y){
  lsa<-lsa(tfidf_2,dim=y)
  words.df_5<-as.data.frame(as.matrix(lsa$dk))
  K_Fold_Data<-cbind(output=data$over.b,words.df)
  K_Fold_Data$output<-as.factor(K_Fold_Data$output)
  K_Fold_Data$output<-make.names(K_Fold_Data$output)
  #traing the model and looking at the results.
  mod_fit <- train(output~.,data =K_Fold_Data,method = "glm",trControl = ctrl)
  print(mod_fit$results)
}

df<-data.frame(matrix(nrow=22, ncol=20))

Kfold_6<-kfold_LSA(6)
rbind(df,6)<-Kfold_6$results[1,]
View(df)

require(rbind)

Lsa_results_vector<-
for(i in 1:20){
  Kfold_i<-kfold_LSA(i)
  df[i,]<-Kfold_i$results[1,]
}


words.df_5<-as.data.frame(as.matrix(lsa_5$dk))

##Correct K-Fold with right measure
library(caret)
library(MLmetrics)
#Creating dataset for K-fold and making adjustments for training the model
K_Fold_Data<-cbind(output=data$over.b,words.df)
K_Fold_Data$output<-as.factor(K_Fold_Data$output)
K_Fold_Data$output<-make.names(K_Fold_Data$output)

#Creating the list for the output metrix
MySummary  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}
#Implementing CV
ctrl <- trainControl(method = "cv",number = 10,savePredictions = TRUE, summaryFunction = MySummary,classProbs = TRUE)

#traing the model and looking at the results.
mod_fit <- train(output~.,data =K_Fold_Data,method = "glm",trControl = ctrl)
mod_fit$results
View(mod_fit$results)

rbind(mod_fit$metric,0)
ls(mod_fit$results)
