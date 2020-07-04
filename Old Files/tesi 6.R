#K-fold 
library(boot)

require(caret)
flds <- createFolds(data$over.b, k = 10, list = TRUE, returnTrain = FALSE)
names(flds)[1] <- "Fold01"

for (i in 1:10) {
  k_trainData=cbind(label=data$over.b[flds[-i]],words.df[flds[-i],])
  glm.fit=glm(label ~.,data=k_trainData, family='binomial') 
  
}
glm.fit=glm(label ~.,data=trainData, family='binomial')

K_Fold_Data<-cbind(output=data$over.b,words.df)
library(caret)
class(K_Fold_Data$output)
K_Fold_Data$output<-as.factor(K_Fold_Data$output)
model<-train(output~.,data=K_Fold_Data,method="glm",trControl=trainControl(method = "cv", number = 10,verboseIter = TRUE))
model

model<-train(output~.,data=K_Fold_Data2,method="glm",trControl=trainControl(method = "cv", number = 10,verboseIter = TRUE, classProbs = TRUE), metric="ROC")
levels(K_Fold_Data$output) <- c(0,1)
K_Fold_Data$output


MySummary  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

ctrl <- trainControl(method = "cv",number = 10,savePredictions = TRUE, summaryFunction = MySummary,classProbs = TRUE)

#F means F_meas
mod_fit <- train(output~.,data =K_Fold_Data2,method = "glm",trControl = ctrl)

K_Fold_Data2<-K_Fold_Data
K_Fold_Data2$output<-make.names(K_Fold_Data2$output)

install.packages("MLmetrics")


#ultimi progetti non definitivi
for_lift <- data.frame(predd = mod_fit$pred$obs, Log = mod_fit$pred$R)
lift_obj <- lift(predd ~ Log, data = for_lift, class = "X1")

class(mod_fit$pred$obs)
mod_fit$pred$R<-as.factor(mod_fit$pred$R)
library(ggplot2)

mod_fit <- train(output~.,data =K_Fold_Data2,method = "glm",trControl = ctrl)
