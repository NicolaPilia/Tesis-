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

#using this code to create the df DataFrame to compare different numbers of LSA
mod_fit <- train(output~.,data =K_Fold_Data,method = "glm",trControl = ctrl)
df[20,]<- mod_fit$results[1,]

#making adjustments at the df
df[,1]<-1:20
colnames(df)[1]<-"number of LSA"

#Creating the plot for the LSA CV
plot(df$number_of_LSA,df$Accuracy,type='l',xaxt='n',xlab = "Number of LSA", ylab="Metrics",ylim = c(0.6,0.85), lwd=2, col="blue", main = "Figure: 1")
lines(df$number_of_LSA,df$Spec,col="green", lwd=2,)
lines(df$number_of_LSA,df$Sens,col="red", lwd=2)
legend("bottomright",legend=c("Accuracy","Specificity","Sensitivity"), col=c("blue","green","red"),pch = c(20,20,20), bty="n")
axis(1, xaxp=c(1, 20,19), las=1,xlab = "Number of LSA")
abline(h=6, v=6, col="gray47", lty=3)

#sensitivity -> 0
#specificity -> 1


