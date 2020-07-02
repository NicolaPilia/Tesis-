View(data)
#aggiungo una colonna e rendo binaria la variabile overall rating
data$over.b<-0

data$over.b<-ifelse(data$overall.ratings>2.5,1,0)
#summary(data$over.b)  

#sample 60% training data
training<-sample(c(1:810), 0.6*810)
training

#run logistic model on training
trainData = cbind(label=data$over.b[training],words.df[training,])
reg<-glm(label ~.,data=trainData, family='binomial')
summary(reg)

#compute accuracy on validation set
ValidData<-cbind(label=data$over.b[-training],words.df[-training,])
pred<-predict(reg,newdata=ValidData,type='response')

#produce confusion matrix
library(caret)
confusionMatrix(ifelse(pred>0.5,1,0), data$over.b[-training])



predd<-ifelse(pred>0.5,1,0)
nrow(pred)
View(pred)
levels(pred$V1)

pred_factor <- factor(ifelse(pred>0.5,1,0), levels = 1:324)
over.b_factor<-factor(data$over.b[-training], levels = 1:324)

confusionMatrix(pred_factor, over.b_factor)
is.na(pred) <- 0

replace(pred_factor, pred_factor=="<NA>", 0)

over.b_vector<-as.vector(over.b_factor)
over.b_vector[is.na(over.b_vector)] <- 0

pred_vector<-as.vector(pred_factor)
pred_vector[is.na(pred_factor)] <- 0
as.factor(pred_factor)
pred_factor<-as.factor(pred_vector)

over.b_factor<-as.factor(over.b_vector)
