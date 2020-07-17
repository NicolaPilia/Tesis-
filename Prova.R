## Loading and cleaning the data
#x <- readLines("netflix.csv")
data<- read.csv("netflix.csv", header =TRUE, sep=";")
#Viewing complete dataset
View(data)
#str(data)
#Translating French observations
levels(data$pros) <- c(levels(data$pros), "Pleasant Hobby Free choice Great quality Languages","Learn to work on pressure. Personal development, team spirit, enthusiasm")
data[810,"pros"]<-"Pleasant Hobby Free choice Great quality Languages"
data[809,"pros"]<-"Learn to work on pressure. Personal development, team spirit, enthusiasm"
levels(data$cons) <- c(levels(data$cons), "I do not know it","Stimulate to do much more than before")
data[810,"cons"]<-"I do not know it"
data[809,"cons"]<-"Stimulate to do much more than before"

#Separating text variables to other variables
cons<-data$cons
pros<-data$pros

View(pros)

#Converting foreign characters into english and dropping french rows
library(gsubfn)

cons<-gsub("Ç","c",cons)
cons<-gsub("¸",",",cons)
cons<-gsub("ƒ","f",cons)
cons<-gsub("ÿ","y",cons)
cons<-gsub("¶","",cons)
cons<-gsub("½","",cons)

pros<-gsub("Ç","c",pros)
pros<-gsub("¸",",",pros)
pros<-gsub("ƒ","f",pros)
pros<-gsub("ÿ","y",pros)
pros<-gsub("¶","",pros)
pros<-gsub("½","",pros)

#creating Corpus
library(tm)
corp_p<-Corpus(VectorSource(pros))
corp_c<-Corpus(VectorSource(cons))

#Removing whitespace, punctuation and numbers

corp_p<-tm_map(corp_p, stripWhitespace)
corp_p<-tm_map(corp_p, removePunctuation)
corp_p<-tm_map(corp_p, removeNumbers)

corp_c<-tm_map(corp_c, stripWhitespace)
corp_c<-tm_map(corp_c, removePunctuation)
corp_c<-tm_map(corp_c, removeNumbers)

#removing stopwords
corp_p<-tm_map(corp_p, removeWords,stopwords("english"))
corp_c<-tm_map(corp_c, removeWords,stopwords("english"))

#stemming the corpuses
corp_p<-tm_map(corp_p,stemDocument)
corp_c<-tm_map(corp_c,stemDocument)

View(corp_p$content)

#modifing words to differentiate between positive and negative terms and unify the two variables, pros and cons, in the same variable.
library(stringr)
corp_c$content<- str_replace_all(corp_c$content, "(\\b\\w)", 'C_\\1')
corp_p$content<- str_replace_all(corp_p$content, "(\\b\\w)", 'P_\\1')
corp<-corp_c
corp$content<-paste(corp_c$content,corp_p$content)
View(corp$content)

#creating TermDocumentMatrix
tdm<-TermDocumentMatrix(corp)

#TF-IDF
tfidf<- weightTfIdf(tdm)
tfidf_df<-as.data.frame(as.matrix(tfidf))

View(tfidf$dimnames$Terms)

#TF-IDF and latent semantic analysis with 6 components (the evaluation of the optimal number of
# LSA component is in the notebook called "CV for LSA")
library(lsa)
lsa.tfidf<-lsa(tfidf,dim=20)

words.df<-as.data.frame(as.matrix(lsa.tfidf$dk))
View(words.df)

#Moving LSA to excel
write.table(words.df, file="LSA_6.csv", quote=F, sep=",", dec=".", na="NA", row.names=T, col.names=T)

#Implementing the logistic regression model
#aggiungo una colonna e rendo binaria la variabile overall rating
data$over.b<-0

data$over.b<-ifelse(data$overall.ratings>3.5,1,0)
summary(data$over.b)  

#sample 60% training data
training<-sample(c(1:810), 0.6*810)
training

#run logistic model on training
trainData = cbind(label=data$over.b[training],words.df[training,])
reg<-glm(label ~V1+V4+V5+V6,data=trainData, family='binomial')
summary(reg)

#compute accuracy on validation set
ValidData<-cbind(label=data$over.b[-training],words.df[-training,])
pred<-predict(reg,newdata=ValidData,type='response')

#confusion matrix
library(caret)

pred_factor <- factor(ifelse(pred>0.5,1,0), levels = 1:323)
pred_vector<-as.vector(pred_factor)
pred_vector[is.na(pred_vector)] <- 0
pred_factor<-as.factor(pred_vector)

over.b_factor<-factor(data$over.b[-training], levels = 1:324)
over.b_vector<-as.vector(over.b_factor)
over.b_vector[is.na(over.b_vector)] <- 0
over.b_factor<-as.factor(over.b_vector)

CM<-confusionMatrix(pred_factor, over.b_factor)
#F1 score form ConfusionMatrix
CM$byClass["F1"]
mean(data$over.b[-training])

library(ROCR)
predd<-prediction(pred,data$over.b[-training])
RP.perf <- performance(predd, "prec", "rec");
plot (RP.perf, main="Precision-Recall Curve", col="blue")
ROC.perf <- performance(predd, "tpr", "fpr")
plot (ROC.perf, main="ROC curve")
abline(coef = c(0,1), col='blue')

install.packages("pROC")
librar
#creating lift chart
#install.packages("gains")
library(gains)

pred_num<-as.numeric(pred)
over.b_num<- as.numeric(as.character(over.b_factor))

gain<-gains(over.b_num,pred_num, groups=10)
barplot(gain$mean.resp/mean(over.b_num), names.arg = gain$depth, xlab="Percentile", ylab= "Mean Response", main="Decile-wise lift chart")

Lift_chart_df<-data.frame(over.b_num,pred)


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
ctrl <- trainControl(method = "cv",number = 10,savePredictions = TRUE, summaryFunction = MySummary,classProbs= TRUE)

df<-data.frame(matrix(nrow=20, ncol=19))
#traing the model and looking at the results.
mod_fit <- train(output~V1+V4+V5+V6,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
mod_fit$results
View(mod_fit$results)

mod_fit2 <- train(output~V1+V2+V3+V4+V5+V6,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
mod_fit2$results
View(mod_fit$results)

#Creating a ConfusionMatrix for interpreting the results
confusionMatrix(mod_fit$pred$pred, mod_fit$pred$obs)


#Comparing two models with 4 and 6 components
df2<-data.frame(matrix(nrow=2, ncol=19))
df2[1,]<-mod_fit$results[1,]
df2[2,]<-mod_fit2$results[1,]
names(df2)<-colnames(mod_fit$results)
df2[,1]<-c("4 components","6 components")
colnames(df2)[1]<-"Model"

write.table(df2, file="df2.csv", quote=F, sep=",", dec=".", na="NA", row.names=T, col.names=T)

# Grouped Bar Plot
counts <- table(df2$Accuracy)
barplot(counts, main="Model comparison",
        xlab="parameter", col=c("darkblue","red"),
        legend = rownames(round(counts)), beside=TRUE, )


#using this code to create the df DataFrame to compare different numbers of LSA
mod_fit <- train(output~V1,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[1,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[2,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[3,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[4,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[5,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[6,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[7,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[8,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[9,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[10,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[11,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[12,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[13,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[14,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[15,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[16,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[17,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[18,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[19,]<- mod_fit$results[1,]
mod_fit <- train(output~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
df[20,]<- mod_fit$results[1,]



#making adjustments at the df
names(df)<-colnames(mod_fit$results)
df[,1]<-1:20
colnames(df)[1]<-"number_of_LSA"

#Creating the plot for the LSA CV
plot(df$number_of_LSA,df$Accuracy,type='l',xaxt='n',xlab = "Number of LSA", ylab="Metrics",ylim = c(0.6,0.85), lwd=2, col="blue", main = "Figure: 1")
lines(df$number_of_LSA,df$Spec,col="green", lwd=2,)
lines(df$number_of_LSA,df$Sens,col="red", lwd=2)
legend("bottomright",legend=c("Accuracy","Specificity","Sensitivity"), col=c("blue","green","red"),pch = c(20,20,20), bty="n")
axis(1, xaxp=c(1, 20,19), las=1,xlab = "Number of LSA")
abline(h=6, v=6, col="gray47", lty=3)


#RANDOM FOREST
library(randomForest)
#creating a df with separate words and making some adjustments
rf.df<-as.data.frame(as.matrix(tfidf))
rf.df<-t(rf.df)
rf.df[over.b]<-data$over.b      
