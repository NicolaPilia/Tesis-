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

#confusion matrix
library(caret)

pred_factor <- factor(ifelse(pred>0.5,1,0), levels = 1:324)
pred_vector<-as.vector(pred_factor)
pred_vector[is.na(pred_vector)] <- 0
pred_factor<-as.factor(pred_vector)

over.b_factor<-factor(data$over.b[-training], levels = 1:324)
over.b_vector<-as.vector(over.b_factor)
over.b_vector[is.na(over.b_vector)] <- 0
over.b_factor<-as.factor(over.b_vector)

confusionMatrix(pred_factor, over.b_factor)

#creating lift chart
#install.packages("gains")
library(gains)

pred_num<-as.numeric(pred)
over.b_num<- as.numeric(as.character(over.b_factor))

gain<-gains(over.b_num,pred_num, groups=10)
barplot(gain$mean.resp/mean(over.b_num), names.arg = gain$depth, xlab="Percentile", ylab= "Mean Response", main="Decile-wise lift chart")

Lift_chart_df<-data.frame(over.b_num,pred)


