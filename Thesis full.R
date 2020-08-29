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

View(corp_c$content)

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
reg<-glm(label ~V1+V2+V3+V4+V5+V6,data=trainData, family='binomial')
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

confusionMatrix(pred_factor, over.b_factor)

#creating lift chart
#install.packages("gains")
library(gains)

pred_num<-as.numeric(pred)
over.b_num<- as.numeric(as.character(over.b_factor))

gain<-gains(over.b_num,pred_num, groups=10)
barplot(gain$mean.resp/mean(over.b_num), names.arg = gain$depth, xlab="Percentile", ylab= "Mean Response", main="Decile-wise lift chart")

Lift_chart_df<-data.frame(over.b_num,pred)

