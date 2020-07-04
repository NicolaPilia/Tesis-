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
mod_fit <- train(output~.,data =K_Fold_Data2,method = "glm",trControl = ctrl)
mod_fit$results

##Other notes
#F means F_meas

for_lift <- data.frame(predd = mod_fit$pred$obs, Log = mod_fit$pred$R)
lift_obj <- lift(predd ~ Log, data = for_lift, class = "X1")

class(mod_fit$pred$obs)
mod_fit$pred$R<-as.factor(mod_fit$pred$R)
library(ggplot2)

mod_fit <- train(output~.,data =K_Fold_Data2,method = "glm",trControl = ctrl)
