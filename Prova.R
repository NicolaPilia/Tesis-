## Loading and cleaning the data
#x <- readLines("netflix.csv")
data<- read.csv("netflix.csv", header =TRUE, sep=";")
#Viewing complete dataset
View(data$cons)
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
cons<-gsub("guis","guys",cons)
cons<-gsub("ppl","people",cons)
cons<-gsub("mgr","manager",cons)
cons<-gsub("manger","manager",cons)
cons<-gsub("guage","gauge",cons)
cons<-gsub("imposssible","impossible",cons)
cons<-gsub("disscussion","discussion",cons)
cons<-gsub("employess","employees",cons)
cons<-gsub("insufficiect","insufficient",cons)
cons<-gsub("mgt","management",cons)
cons<-gsub("WIshy washy","wishiwashy",cons)

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
pros<-gsub("ppl","people",pros)
pros<-gsub("saavy","savvy",pros)
pros<-gsub("mgr","manager",pros)
pros<-gsub("Cluture","culture",pros)
pros<-gsub("prettt","pretty",pros)

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

View(corp_c$content)

#modifing words to differentiate between positive and negative terms and unify the two variables, pros and cons, in the same variable.
library(stringr)
corp_c$content<- str_replace_all(corp_c$content, "(\\b\\w)", 'c_\\1')
corp_p$content<- str_replace_all(corp_p$content, "(\\b\\w)", 'p_\\1')
corp<-corp_c
corp$content<-paste(corp_c$content,corp_p$content)
View(corp$content)


#creating TermDocumentMatrix
tdm<-TermDocumentMatrix(corp)
View(tdm$dimnames$Terms)
#TF-IDF
tfidf<- weightTfIdf(tdm)

#dropping empty documents
colTotals <- apply(tfidf , 2, sum)
tfidf <- tfidf[ , colTotals> 0]

#converting to dataframe
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
lsa_inter<-as.data.frame(as.matrix(lsa.tfidf$tk))
write.table(lsa_inter, file="LSA_I2.csv", quote=F, sep=",", dec=".", na="NA", row.names=T, col.names=T)

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
reg<-glm(label~V1+V4+V5+V6,data=trainData, family='binomial')
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
mod_fit <- train(output~.,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
mod_fit$results
View(mod_fit$results)

mod_fit2 <- train(output~V2+V3+V8+V12+V13+V15+V17,data =K_Fold_Data,method = "glm",trControl = ctrl, family="binomial")
mod_fit2$results
View(mod_fit2$results)

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
mod_fit <- train(output~V1,data =K_Fold_Data,met hod = "glm",trControl = ctrl, family="binomial")
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

rf.model<-randomForest(y=rf.output[training],x=rf.df[training,], mtry=74 , importance=TRUE)
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

#rf.ValidData2<-rf.df[training,]
#rf.pred2<-predict(rf.model,newdata=rf.ValidData2,type='response')
#rf.confmat2<-confusionMatrix(rf.pred2,rf.output[training])

rf.output_cv<-make.names(rf.output)
rf.model_cv <- train(y=rf.output_cv,x=rf.df,method = "rf",trControl = ctrl)

variable.importance<-as.data.frame(importance(rf.model))
variable.importance<-variable.importance[order(variable.importance$MeanDecreaseGini, decreasing = TRUE),] 



variab_imp<-as.data.frame(varImp(reg))
variab_imp[,2]<-rownames(variab_imp)
variab_imp<-variab_imp[,c("V2","Overall")]
rownames(variab_imp)<-1:20
colnames(variab_imp)[1]<-"Predictors"
variab_imp<-variab_imp[order(variab_imp$Overall, decreasing =TRUE),]


library(ggplot2)
library(cowplot)
library(dplyr)
library(reshape2)

df3 <- data.frame(n_comp=c('20 components','5 components'),
                 Accuracy=c(0.7754178,0.7716294),
                 Sens=c(0.8006006,0.7783033),
                 Spec=c(0.7548485,0.7663636))
reshape2::melt(df3, id.vars = "n_comp") %>% 
  mutate(n_comp = relevel(factor(n_comp), "5 components")) %>% 
  ggplot(aes(x=variable, y=value, fill=n_comp, 
             label = scales::percent(value, accuracy=.1), width=.6)) +
  geom_bar(stat='identity', position=position_dodge2(width=.8, padding=.2)) +
  geom_text(position = position_dodge(.6), vjust=-0.5, size=5) +
  theme_minimal_hgrid()+
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        text = element_text(size = 18),
        axis.text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5)) +
  labs(x=NULL, y=NULL, title = "Model Comparison")+
  coord_cartesian(ylim = c(.6, .85)) +
  scale_fill_manual(values = c("#4472c4", "#eb7c31"))
