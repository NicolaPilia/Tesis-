## Loading and cleaning the data
#x <- readLines("netflix.csv")
data<- read.csv("netflix.csv", header =TRUE, sep=";")
#Viewing complete dataset
View(data)
str(data)
#Translating French observations
levels(data$pros) <- c(levels(data$pros), "Pleasant Hobby Free choice Great quality Languages","Learn to work on pressure. Personal development, team spirit, enthusiasm")
data[810,"pros"]<-"Pleasant Hobby Free choice Great quality Languages"
data[809,"pros"]<-"Learn to work on pressure. Personal development, team spirit, enthusiasm"
levels(data$cons) <- c(levels(data$cons), "I do not know it","Stimulate to do much more than before")
data[810,"cons"]<-"I do not know it"
data[809,"cons"]<-"Stimulate to do much more than before"

data<-data[c(-52,-57,-109,-114,-258,-303,-309,-449),]
rownames(data) <- 1:nrow(data)
#Separating text variables to other variables
cons<-data$cons
pros<-data$pros

#Converting foreign characters into english and dropping french rows
library(gsubfn)

cons<-gsub("Ç","c",cons)
cons<-gsub("¸",",",cons)
cons<-gsub("ƒ"," ",cons)
cons<-gsub("ÿ","",cons)
cons<-gsub("¶","",cons)
cons<-gsub("½","",cons)
cons<-gsub("","",cons)
cons<-gsub("ù","",cons)
cons<-gsub("/"," ",cons)
cons<-gsub("-"," ",cons)
cons<-gsub("[.]"," ",cons)
cons<-gsub(","," ",cons)
cons<-gsub("[+]"," ",cons)
cons<-gsub("ý","",cons)
cons<-gsub("<","",cons)
cons<-gsub("[(]"," ",cons)
cons<-gsub("sloooooow","slow",cons)
cons<-gsub("\\bguis\\b","guys",cons)
cons<-gsub("\\bppl\\b","people",cons)
cons<-gsub("\\bmgr\\b","manager",cons)
cons<-gsub("\\bmanger\\b","manager",cons)
cons<-gsub("\\bguage\\b","gauge",cons)
cons<-gsub("imposssible","impossible",cons)
cons<-gsub("disscussion","discussion",cons)
cons<-gsub("employess","employees",cons)
cons<-gsub("insufficiect","insufficient",cons)
cons<-gsub("\\bmgt\\b","management",cons)
cons<-gsub("WIshy washy","wishiwashy",cons)
cons<-gsub("\\bmgmt\\b","management",cons)
cons<-gsub("\\bfavorite\\b","favourite",cons)
cons<-gsub("\\bmarketpay\\b","market pay",cons)

pros<-gsub("Ç","c",pros)
pros<-gsub("¸",",",pros)
pros<-gsub("ƒ"," ",pros)
pros<-gsub("ÿ","",pros)
pros<-gsub("¶","",pros)
pros<-gsub("","",pros)
pros<-gsub("½","",pros)
pros<-gsub("ù","",pros)
pros<-gsub("/"," ",pros)
pros<-gsub("-"," ",pros)
pros<-gsub("[.]"," ",pros)
pros<-gsub(","," ",pros)
pros<-gsub("[+]"," ",pros)
pros<-gsub("ý","",pros)
pros<-gsub("<","",pros)
pros<-gsub("[(]"," ",pros)
pros<-gsub("\\bppl\\b","people",pros)
pros<-gsub("\\bsaavy\\b","savvy",pros)
pros<-gsub("\\bmgr\\b","manager",pros)
pros<-gsub("\\bCluture\\b","culture",pros)
pros<-gsub("\\bprettt\\b","pretty",pros)
pros<-gsub("\\bmgmt\\b","management",pros)
pros<-gsub("\\bfavorite\\b","favourite",pros)
pros<-gsub("\\bmarketpay\\b","market pay",pros)

#creating Corpus
library(tm)
corp_p<-Corpus(VectorSource(pros))
corp_c<-Corpus(VectorSource(cons))

#Removing whitespace, punctuation and numbers

corp_p<-tm_map(corp_p, stripWhitespace)
corp_p<-tm_map(corp_p, removePunctuation)
corp_p<-tm_map(corp_p, removeNumbers)
corp_p<-tm_map(corp_p, tolower)

corp_c<-tm_map(corp_c, stripWhitespace)
corp_c<-tm_map(corp_c, removePunctuation)
corp_c<-tm_map(corp_c, removeNumbers)
corp_c<-tm_map(corp_c, tolower)

#removing stopwords
corp_p<-tm_map(corp_p, removeWords,stopwords("english"))
corp_c<-tm_map(corp_c, removeWords,stopwords("english"))
corp_p<-tm_map(corp_p, removeWords,stopwords("SMART"))
corp_c<-tm_map(corp_c, removeWords,stopwords("SMART"))

#stemming the corpuses
corp_p<-tm_map(corp_p,stemDocument)
corp_c<-tm_map(corp_c,stemDocument)

#re-removing stopwords using SMART
corp_p<-tm_map(corp_p, removeWords,stopwords("SMART"))
corp_c<-tm_map(corp_c, removeWords,stopwords("SMART"))

#modifing words to differentiate between positive and negative terms and unify the two variables, pros and cons, in the same variable.
library(stringr)
corp_c$content<- str_replace_all(corp_c$content, "(\\b\\w)", 'c_\\1')
corp_p$content<- str_replace_all(corp_p$content, "(\\b\\w)", 'p_\\1')
corp<-corp_c
corp$content<-paste(corp_c$content,corp_p$content)

corp <- tm_map(corp, removeWords, pulizia2)
corp<-tm_map(corp, stripWhitespace)

tdm<-TermDocumentMatrix(corp)

#TF-IDF
tfidf<- weightTfIdf(tdm)

#converting to dataframe
tfidf_df<-as.data.frame(as.matrix(tfidf))

#TF-IDF and latent semantic analysis with 6 components (the evaluation of the optimal number of
# LSA component is in the notebook called "CV for LSA")
library(lsa)
lsa.tfidf<-lsa(tfidf,dim=20)

words.df<-as.data.frame(as.matrix(lsa.tfidf$dk))

#Moving LSA to excel
#LSA with documents
write.table(words.df, file="LSA_6.csv", quote=F, sep=",", dec=".", na="NA", row.names=T, col.names=T)
#LSA with words
lsa_inter<-as.data.frame(as.matrix(lsa.tfidf$tk))
lsa_inter<-lsa_inter[c(7,3,13,4,10,1)]
write.table(lsa_inter, file="LSA_I3.csv", quote=F, sep=",", dec=".", na="NA", row.names=T, col.names=T)

#Implementing the logistic regression model
#aggiungo una colonna e rendo binaria la variabile overall rating
data$over.b<-0

data$over.b<-ifelse(data$overall.ratings>3.5,1,0)
summary(data$over.b)  

#sample 60% training data
training<-sample(c(1:802), 0.6*802)
training

#run logistic model on training
trainData = cbind(label=data$over.b[training],words.df[training,])
reg<-glm(label~V7+V3+V13+V4+V10+V1,data=trainData, family='binomial')
summary(reg)

reg<-glm(label~.,data=trainData, family='binomial')
#compute accuracy on validation set
ValidData<-cbind(label=data$over.b[-training],words.df[-training,])
pred<-predict(reg,newdata=ValidData,type='response')

#confusion matrix
library(caret)

pred_factor <- factor(ifelse(pred>0.5,1,0))

over.b_factor<-factor(data$over.b[-training])

CM<-confusionMatrix(pred_factor, over.b_factor)
CM
CM$byClass["Sensitivity"]
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
mod_fit <- train(output~.,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
mod_fit$results
View(mod_fit$results)

mod_fit2 <- train(output~V7+V3+V13+V4+V10+V1,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
mod_fit2$results
View(mod_fit2$results)

#Creating a ConfusionMatrix for interpreting the results
confusionMatrix(mod_fit$pred$pred, mod_fit$pred$obs)


#Comparing two models with 5 and 20 components
df2<-data.frame(matrix(nrow=2, ncol=19))
df2[1,]<-mod_fit$results[1,]
df2[2,]<-mod_fit2$results[1,]
names(df2)<-colnames(mod_fit$results)
df2[,1]<-c("20 components","5 components")
colnames(df2)[1]<-"Model"
modelcomp<-df2[,c(1,2,5,6)]

df2[,-1]<-df2[,-1]*100
df2<-as.numeric(df2[,1])

write.table(df2, file="df2.csv", quote=F, sep=",", dec=".", na="NA", row.names=T, col.names=T)

# Grouped Bar Plot
counts <- table(df2)
barplot(counts, main="Model comparison",
        xlab="parameter", col=c("darkblue","red"),
        legend = rownames(round(counts)), beside=TRUE, )
barplot(counts, main="Car Distribution",
        xlab="Number of Gears") 

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
rf.df<-as.data.frame(t(rf.df))
rf.output<-as.factor(data$over.b)

rf.model<-randomForest(y=rf.output[training],x=rf.df[training,], mtry=66 , importance=TRUE, localImp = TRUE)
#another way of doing it
#rf.model<-randomForest(y=rf.output,x=rf.df,subset = training, mtry=74 , ntree=500, importance=TRUE)
rf.model
#note on confusion matrix-> You have to load randomForest before launching r.model
#rows are actual values and columns are predicted values

#measuring oob error rate to understand if tree number is correct
oob.error.data<-data.frame(
  Trees=rep(1:nrow(rf.model$err.rate), times=3), 
  Type=rep(c("OOB", "0", "1"), each=nrow(rf.model$err.rate)),
  Error=c(rf.model$err.rate[,"OOB"],
          rf.model$err.rate[,"0"],
          rf.model$err.rate[,"1"]))

#plotting oob error rate
library(ggplot2)
library(cowplot)

ggplot(data=oob.error.data, aes(x=Trees,y=Error))+
  geom_line(aes(color=Type))

#variable importance
importance(rf.model)

rf.ValidData<-rf.df[-training,]
rf.pred<-predict(rf.model,newdata=rf.ValidData,type='response')
rf.confmat<-confusionMatrix(rf.pred,rf.output[-training])
rf.confmat
#rf.ValidData2<-rf.df[training,]
#rf.pred2<-predict(rf.model,newdata=rf.ValidData2,type='response')
#rf.confmat2<-confusionMatrix(rf.pred2,rf.output[training])

rf.output_cv<-make.names(rf.output)
rf.model_cv <- train(y=rf.output_cv,x=rf.df,method = "rf",trControl = ctrl)

variable.importance<-as.data.frame(importance(rf.model))
variable.importance<-variable.importance[order(variable.importance$MeanDecreaseGini, decreasing = TRUE),]

#min depht distribution
library(randomForestExplainer)
min_depth_frame <- min_depth_distribution(rf.model)
save(min_depth_frame, file = "min_depth_frame.rda")
plot_min_depth_distribution(min_depth_frame)

plot_min_depth_distribution(min_depth_frame, mean_sample = "all_trees", k = 10)

#variable importance
importance_frame <- measure_importance(rf.model)

plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

plot_multi_way_importance(importance_frame, x_measure = "accuracy_decrease", y_measure = "gini_decrease", size_measure = "p_value", no_of_labels = 5)

plot_multi_way_importance(importance_frame, x_measure = "accuracy_decrease", y_measure = "gini_decrease",  no_of_labels = 10)

library(ggplot2)
library(forcats)
# Basic barplot
variable.importance<-variable.importance[order(variable.importance$MeanDecreaseGini, decreasing = TRUE),]
mdg<-variable.importance[1:10,c(1,4)]
mdg<-mdg[order(mdg$MeanDecreaseGini, decreasing = FALSE),]

mdg_plot<-ggplot(data=mdg, x=rownames(mdg), y=mdg$MeanDecreaseGini ,aes(x=fct_inorder(rownames(mdg)),y=MeanDecreaseGini, label = MeanDecreaseGini)) +
  geom_bar(stat="identity")
mdg_plot
mdg_plot + coord_flip()+ labs(x="Variabili", y="Mean Decreased Gini") + geom_text(aes(label = round(MeanDecreaseGini, 1), hjust = -0.2)) + ylim(NA, 10)
mdg_plot 

variable.importance<-variable.importance[order(variable.importance$MeanDecreaseAccuracy, decreasing = TRUE),]
mda<-variable.importance[1:10,c(1,3)]
mda<-mda[order(mda$MeanDecreaseAccuracy, decreasing = FALSE),]

mda_plot<-ggplot(data=mda, x=rownames(mda), y=mda$MeanDecreaseAccuracy ,aes(x=fct_inorder(rownames(mdg)),y=MeanDecreaseAccuracy, label = MeanDecreaseAccuracy)) +
  geom_bar(stat="identity")
mda_plot
mda_plot + coord_flip()+ labs(x="Variabili", y="Mean Decreased Accuracy") + geom_text(aes(label = round(MeanDecreaseAccuracy, 1), hjust = -0.2)) + ylim(NA, 16)

